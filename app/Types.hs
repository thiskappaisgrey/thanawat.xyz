{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson as A
import qualified Data.Map as M
import Data.Org (OrgDoc, OrgFile (OrgFile), orgFile)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake.Classes
import GHC.Generics (Generic)

-- avoid circular dependency

-- | Data for the index page
newtype IndexInfo = IndexInfo
  { posts :: [Post]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post = Post
  { title :: T.Text,
    -- TODO the "content" of the post is really the unescaped HTML ouput
    -- , content :: String
    -- TODO add summary + content
    url :: T.Text,
    date :: T.Text
    -- , image   :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Data for an About Page
data AboutSection = AboutSection
  { sectionHeader :: T.Text,
    sectionContent :: OrgFile,
    sectionImagePath :: Maybe T.Text
  }
  deriving (Show)

data About = About
  { aboutPreamble :: OrgFile,
    aboutSections :: [AboutSection]
  }
  deriving (Show)

data Atom = Atom
  { _title :: Text,
    _updated :: Text,
    _atomUrl :: Text,
    _posts :: [Post],
    _domain :: Text
  }
