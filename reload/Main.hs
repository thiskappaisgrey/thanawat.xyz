{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import qualified Network.WebSockets as WS
import System.FSNotify
import Language.Haskell.Ghcid


type Connections = MVar [WS.Connection]
-- sends a message to the browser when reload is required
-- this will forever reload the browser which is bad..
-- TODO right now I'm not keeping track of all the connections in an MVar, but I should be doing that
-- Maybe this code is buggy is because if there are no clients, then the "takeMVar doneCompile" won't run, which means that
reloadBrowserOnFileChange :: MVar () -> Connections ->  IO ()
reloadBrowserOnFileChange doneCompile connsMvar = forever $ do
  takeMVar doneCompile
  putStrLn "Refreshing all the clients"
  conns <- readMVar connsMvar
  forM_ conns (\conn -> WS.sendTextData conn ("refresh the browser" :: T.Text))
  modifyMVar_ connsMvar (\_ -> return []) -- Bug with the live reload server!! the clients never get cleared after reloading

static :: Application
static = staticApp $ defaultFileServerSettings "./build"

app :: MVar () -> Connections -> Application
app doneCompile connsMVar = websocketsOr defaultConnectionOptions wsApp static
  where
    wsApp :: WS.ServerApp
    wsApp pending_conn = do
      conn <- acceptRequest pending_conn
      -- put the handler in a pinging thread
      WS.withPingThread conn 30 (return ()) $ do -- Maybe I need to move the forever here?? not sure what's going on..
        emp <- isEmptyMVar connsMVar
        putStrLn $ mconcat ["connections is empty: ", show emp]
        putStrLn "Accepting connection from browser"
        conns <- modifyMVar connsMVar (\s -> do
                                          let s' = conn:s -- only do the computation once
                                          return (s',s')) -- add the connection to the connections MVar
        putStrLn $ mconcat ["There are now: ", show $ length conns, " clients connected" ]
        reloadBrowserOnFileChange doneCompile connsMVar
reloadGhciFileChange :: MVar Event -> MVar () -> Connections -> Ghci -> IO ()
reloadGhciFileChange fileChanged doneCompile connsMVar ghci = do
  e <- takeMVar fileChanged
  putStrLn ("Restarting ghci because: " <> show e)
  msg <- reload ghci -- TODO Pattern match on the msg
  print msg -- TODO problematic part
  execMsg <- exec ghci "Main.main"
  forM_ execMsg putStrLn
  conns <- readMVar connsMVar
  let emp = null conns
  putStrLn ("There are connections: " <> show emp)
  unless emp $ putMVar doneCompile ()
ghciStart :: IO Ghci
ghciStart = do
  (ghci, load) <- startGhci "cabal new-repl build-site" (Just ".") $ const putStrLn
  forM_ load print
  return ghci
-- TODO write a file watcher that watches for changes in a directory
main :: IO ()
main = do
  fileMvar <- newEmptyMVar :: IO (MVar Event) -- detects if file changed
  doneCompile <- newEmptyMVar :: IO (MVar ())  -- after recompiling, put into this mvar and reload
  connsMVar <- newEmptyMVar:: IO Connections  -- list of connected clients
  putMVar connsMVar []
  tid <- forkIO $ do
    ghci <- ghciStart
    forever $ reloadGhciFileChange fileMvar doneCompile connsMVar ghci
  putStrLn ("Running ghci in thread: " <> show tid)
  tid2 <- forkIO $ run 3000 $ app doneCompile connsMVar
  putStrLn ("Running the file server on thread: " <> show tid2)

  withManager $ \mgr -> do
    -- start a watching job (in the background)
    putStrLn "Watching ./build"

    watchDir
      mgr -- manager
      "./app" -- directory to watch
      (const True) -- predicate
      (\event -> do
          print event
          putMVar fileMvar event
          )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
