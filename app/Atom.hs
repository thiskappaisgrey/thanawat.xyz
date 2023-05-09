module Atom where
import qualified Data.Text as T
import qualified Data.Text.Lazy as TZ
import Data.Text (Text)
import Lucid.Base
import Types (Atom(..), Post(..))
import Control.Monad (mapM_, forM_)
import Page (post)
-- Add terms for xml
feed_ :: Term arg result => arg -> result
feed_ = termWith "feed" [xmlns_ "http://www.w3.org/2005/Atom"]
  where
    xmlns_ = term "xmlns"
title_ :: Term arg result => arg -> result
title_ = term "title"
updated_ :: Term arg result => arg -> result
updated_ = term "updated"
author_ :: Term arg result => arg -> result
author_ = term "author"
name_ :: Term arg result => arg -> result
name_ = term "name"
id_ :: Term arg result => arg -> result
id_ = term "id"
entry_ :: Term arg result => arg -> result
entry_ = term "entry"
summary_ :: Term arg result => arg -> result
summary_ = term "summary"

category_ :: Applicative m => Text -> HtmlT m ()
category_ c = with (makeElementNoEnd "input") [term_ c]
  where
    term_ = term "term"
type_ :: Text -> Attribute
type_ = makeAttribute "type"
href_ :: Text -> Attribute
href_ = makeAttribute "href"
rel_ :: Text -> Attribute
rel_ = makeAttribute "rel"

content_ :: Term arg result => arg -> result
content_ = termWith "content" [type_ "html"]

link_ :: Applicative m => [Attribute] -> HtmlT m ()
link_ = with (makeXmlElementNoEnd "link")
postTemplate :: Text -> Post -> Html () 
postTemplate dom p = do
  entry_ $ do
    title_ $ toHtml $ title p
    link_ [href_ $ dom <> "/" <> url p]
    id_ $ toHtml $ dom <> "/" <> url p
    updated_ $ toHtml $ date p
    -- TODO summary / content not added yet..
  
template :: Atom -> Html ()
template a = feed_ $ do
  title_ $ toHtml $ _title a
  -- TODO
  link_ [href_ $ _domain a <> _atomUrl a]
  updated_ $ toHtml $ _updated a
  author_ $ name_ "Thanawat Techaumnuaiwit"
  id_ $ toHtml $ _domain a
  -- TODO for each post, I need to do add the XML feed
  forM_ (_posts a) (postTemplate (_domain a)) 



-- TODO
renderAtomFeed :: Atom -> TZ.Text
renderAtomFeed a = header <> renderText (template a)
  where
    header = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
