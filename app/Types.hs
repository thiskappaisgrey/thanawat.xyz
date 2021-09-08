{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import           Data.Aeson                 as A
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Development.Shake.Classes

-- avoid circular dependency
-- | Data for the index page
data IndexInfo =
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
