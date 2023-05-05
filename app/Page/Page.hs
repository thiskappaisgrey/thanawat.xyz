-- |

module Page (post, wrapPage) where
import Lucid
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Types
import Colors
import Page.Components
import Data.Org (Todo(TODO))
import Control.Monad (when)
  
-- TODO Right now, I have 1 style sheet for thing but I might want to separate it later I guess?
postList :: [Post] -> Html ()
postList postList = let
  postItem :: Post -> Html ()
  postItem post = li_ $ a_ [href_ $ url post] $ do
    toHtml $ title post
    span_ [class_ "date"] $ toHtml $ date post
  in
  div_  [class_ "posts"] $ do
    h1_ "My Posts"
    ul_ $ mapM_ postItem postList

wrapPage :: Bool -> Html () -> String
wrapPage n p = TZ.unpack $ renderText $ baseTemplate "bg-slate-800 text-slate-200" $ do
  when n navBar
  p
-- For rendering individual posts
post :: Html () -> String
post orgHtml = TZ.unpack $ renderText $ baseTemplate "post" orgHtml
