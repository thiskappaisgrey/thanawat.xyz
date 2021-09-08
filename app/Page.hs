{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Page where
import Lucid
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Types
import Clay as C hiding (type_, title, url)

-- I can pull up the metadata into a new type but I'll keep it like this for now
baseTemplate ::  String ->  Html () -> Html ()
baseTemplate bodyClass body =
  let header = head_ $
        meta_ [charset_ "UTF-8"]
        <> meta_ [name_ "description", content_ "thanawat.xyz"]
        <> meta_ [name_ "author", content_ "Thanawat Techaumnuaiwit"]
        <> meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        <> title_ "Thanawat.xyz"
        <> link_ [rel_ "stylesheet", href_ "/styleSheet.css"]
        <> script_ [type_ "text/javascript", src_ "/js/ws.js"] ""
        -- <> link_ [rel_ "stylesheet", href_ "/css/syntax.css"]
  in
  doctype_ <> html_ [lang_ "en"] header <> body_ [class_ $ T.pack bodyClass] body
styleSheet :: Css
styleSheet =  ".post" ?
    do
      -- some comment
      h1 ? do
        textAlign center
        color blue
      h2 ? fontSizeCustom xxLarge
postList :: [Post] -> Html ()
postList postList = let
  postItem :: Post -> Html ()
  postItem post = li_ $ a_ [href_ $ url post] $ do
    toHtml $ title post
    span_ [class_ "date"] $ toHtml $ date post
  in
  do
    h1_ "Hi, My name is Thanawat! Come look at my posts!"
    ul_ $ mapM_ postItem postList

index :: [Post] -> String
index posts = TZ.unpack $ renderText $ baseTemplate "" $ postList posts

post :: Html () -> String
post orgHtml = TZ.unpack $ renderText $ baseTemplate "post" orgHtml
