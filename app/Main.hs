{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Lens

-- in case I want to remove directory recursively on rebuild

import Atom (renderAtomFeed)
import Control.Monad
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Default (def)
import qualified Data.Map as M
import Data.Org as O
import qualified Data.Org.Lucid as LO
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Lazy as TZ
import Data.Validation as V
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import qualified Lucid as L
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Optics.Core ((%), (&), (.~), (^.))
import qualified Page as P
import qualified Page.About as PA
import qualified Page.Index as PI
import System.Directory (removeDirectoryRecursive)
import Types
import qualified Web.Tailwind as Tailwind
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time (getCurrentTime)

outputFolder :: FilePath
outputFolder = "build/"

-- Utility Functions------------------------------------------------------------
-- TODO I need to extract the metadata for the index
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither msg Nothing = Left msg
maybeToEither msg (Just val) = Right val

maybeToValidation :: String -> Maybe a -> Validation OrgMetaError a
maybeToValidation msg Nothing = Failure (OrgMetaError msg)
maybeToValidation msg (Just val) = V.Success val

-- Basically allow you to accumulate the error in a string like:
-- title, date ...
newtype OrgMetaError = OrgMetaError String

instance Semigroup OrgMetaError where
  (OrgMetaError a) <> (OrgMetaError b) = OrgMetaError $ mconcat [a, ", ", b]

instance Monoid OrgMetaError where
  mappend = (<>)
  mempty = OrgMetaError ""

getMetaData :: M.Map T.Text T.Text -> T.Text -> Validation OrgMetaError Post
getMetaData m postUrl =
  Post
    <$> maybeToValidation "title" (M.lookup "TITLE" m)
    <*> pure postUrl
    <*> maybeToValidation "date" (M.lookup "DATE" m)

-- TODO maybe consider writing Text versions of writeFile'?
convertOrg :: T.Text -> T.Text -> Action Post
convertOrg postUrl orgText = do
  let orgFile = O.org orgText
  case orgFile of
    Nothing -> fail "Parsing org file failed"
    Just file -> do
      let m = orgMeta file
      -- Convert a Maybe value to Either value for better error messages
      let metaData = getMetaData m postUrl
      case metaData of
        Failure (OrgMetaError str) -> fail $ mconcat ["Post ", T.unpack postUrl, " is missing: ", str]
        V.Success m -> do
          let post = P.post $ LO.body LO.defaultStyle file
          writeFile' (outputFolder </> T.unpack postUrl) post
          return m

-- Build Rules -----------------------------------------------------------

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.org"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob (This is the Value type)
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
  -- withPostUrl = _Object . at "url" ?~ String postUrl
  -- helper to take the content and output an org file
  a <- convertOrg postUrl $ T.pack postContent
  liftIO . putStrLn $ "Post: " <> show a
  return a

buildIndex :: [Post] -> Action ()
buildIndex post =
  writeFile' (outputFolder </> "index.html") $ P.wrapPage False $ PI.index post

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $
    forP filepaths $ \filepath ->
      copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- Requires a "Partial" type contraint but I don't know which Partial it is..
readFileText :: FilePath -> Action T.Text
readFileText x = need [x] >> liftIO (T.IO.readFile x)

-- TODO get "templates" as org files, filer out the necessary info then do some stuff with it?
buildAbout :: Action ()
buildAbout = do
  -- the wording "template" doesn't exactly make sense here? so use "page" instead?
  aboutContent <- readFileText "./site/page/about.org"
  let orgData = O.org aboutContent
  case orgData of
    Just o -> do
      let oe = PA.orgDocToData o
      case oe of
        Left e ->
          liftIO $ putStrLn ("There was an error with the parsing the org file: " <> show e)
        Right o ->
          writeFile' (outputFolder </> "about.html") $ P.wrapPage True $ PA.about o
    Nothing ->
      liftIO $ putStrLn "About File could not be parsed"

-- build tailwind
compileTailwindCss :: FilePath -> [FilePath] -> Action ()
compileTailwindCss cssPath genPaths = liftIO $
  runStdoutLoggingT $
    do
      Tailwind.runTailwind $
        def
          & Tailwind.tailwindConfig % Tailwind.tailwindConfigContent .~ genPaths
          & Tailwind.tailwindOutput .~ cssPath
          & Tailwind.tailwindMode .~ Tailwind.Production

-- TODO atom
siteAtom :: Atom
siteAtom =
  Atom
    { _title = "thanawat.xyz",
      _atomUrl = "thanawat.xyz/atom.xml",
      _updated = "05-09-2023", -- TODO
      _posts = [],
      _domain = "https://thanawat.xyz"
    }

-- build site atom
buildSiteAtom :: [Post] -> Action ()
buildSiteAtom posts = do
  now <- liftIO getCurrentTime
  writeFile' (outputFolder </> "feed.xml") $
    TZ.unpack $
      renderAtomFeed $
        Atom
          { _title = "thanawat.xyz",
            _atomUrl = "/feed.xml",
            _updated = T.pack $  iso8601Show now,
            _posts = posts,
            _domain = "https://thanawat.xyz"
          } 
    

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  posts <- buildPosts
  buildAbout
  buildIndex posts
  buildSiteAtom posts
  copyStaticFiles
  -- TODO the second argument needs to be hall the haskell files
  compileTailwindCss (outputFolder </> "tailwind.css") ["./app/Page/*.hs"]

-- | Kick it all off
main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeVerbosity = Chatty,
              shakeLintInside = [""]
            }
  -- removeDirectoryRecursive "./.shake" -- TODO remove when I'm done with editing the haskell files
  shakeArgsForward shOpts buildRules
