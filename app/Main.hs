{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.Map as M
import Data.Org as O
import qualified Data.Org.Lucid as LO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TZ
import Data.Validation as V
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import qualified Lucid as L
import qualified Page as P
import Slick
import Types
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import qualified Clay as C
import System.Directory (removeDirectoryRecursive) -- in case I want to remove directory recursively on rebuild

outputFolder :: FilePath
outputFolder = "build/"


-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.org"]
  forP pPaths buildPost

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
  (<>) = mappend

instance Monoid OrgMetaError where
  mappend (OrgMetaError a) (OrgMetaError b) = OrgMetaError $ mconcat [a, ", ", b]
  mempty = OrgMetaError ""

getMetaData :: M.Map T.Text T.Text -> T.Text -> Validation OrgMetaError Post
getMetaData m postUrl = Post <$>
  maybeToValidation "title" (M.lookup "TITLE" m)
  <*> pure postUrl
  <*> maybeToValidation "date" (M.lookup "DATE" m)

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
          writeFile' (outputFolder </> T.unpack postUrl)  post
          return m

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
  convertOrg postUrl $ T.pack postContent

buildIndex :: [Post] -> Action ()
buildIndex post =
  writeFile' (outputFolder </> "index.html") $ P.index post

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*"]
  void $
    forP filepaths $ \filepath ->
      copyFileChanged ("site" </> filepath) (outputFolder </> filepath)
buildCss :: Action ()
buildCss = do
  let cssText = C.renderWith C.compact [] P.styleSheet
  writeFile' (outputFolder </> "styleSheet.css") $ TZ.unpack cssText

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  posts <- buildPosts
  buildIndex posts
  buildCss
  copyStaticFiles


-- | Kick it all off
main :: IO ()
main = do
  let shOpts =
        forwardOptions $
          shakeOptions
            { shakeVerbosity = Chatty,
              shakeLintInside = [""]
            }
  removeDirectoryRecursive "./.shake" -- TODO remove when I'm done with editing the haskell files
  shakeArgsForward shOpts buildRules
