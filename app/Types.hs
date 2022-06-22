{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import           Data.Aeson                 as A
import qualified Data.Text                  as T
import           Data.Org                   (OrgDoc, orgFile, OrgFile (OrgFile))
import           GHC.Generics               (Generic)
import           Development.Shake.Classes
import qualified Data.Map                    as M
-- avoid circular dependency
-- | Data for the index page
newtype IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: T.Text
         -- , content :: String
         , url     ::  T.Text
         , date    ::   T.Text
         -- , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Data for an About Page
data AboutSection = AboutSection {
  sectionHeader :: T.Text
  , sectionContent :: OrgFile 
  , sectionImagePath :: Maybe T.Text
                                 } deriving (Show)
data About = About {
  aboutPreamble :: OrgFile
  , aboutSections :: [AboutSection]
                   } deriving (Show)
