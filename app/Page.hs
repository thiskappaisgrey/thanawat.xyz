{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Page where
import Lucid
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Types
import Clay as C hiding (type_, title, url)
import Colors

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
styleSheet = do
  -- get's rid of default styles
  star ? do
    sym C.margin (px 0)
    sym C.padding (px 0)
    C.verticalAlign (px 0)
    fontColor $ parse nord4
  -- set default body background
  body ? do
    background $ parse nord1
  ".post" ?
    do
      h1 ? do
        textAlign center
        color blue
      h2 ? fontSize (C.rem 2)
  navSheet
  heroSheet
  postSheet
navSheet :: Css
navSheet = nav ? do
  background $ parse nord0
  sym padding (C.rem 1)
  display flex
  justifyContent spaceBetween
  marginBottom (C.rem 1)
  C.div ? do
    display flex
    justifyContent spaceAround
    width (pct 20)
    h1 ? do
      fontSize (C.rem 1.5)
    h2 ? do
      fontSize (C.rem 1.2)
navBar :: Html ()
navBar = nav_ [class_ ".nav"] $ do
  h1_ "Thanawat.xyz"
  div_ $ do
    -- TODO Make these actual links when I actually make the pages
    h2_ "Blog"
    h2_ "My Resume"
heroSheet :: Css
heroSheet = ".hero" ? do
  textAlign center
  lineHeight (C.rem 1.5)
  fontColor $ parse nord6
  width (pct 33)
  marginLeft auto -- margin auto centers things
  marginRight auto
  star ? do
    marginBottom (C.rem 1.5)
  ".spacing" |> star ? do
    marginLeft (C.rem 0.5)
    marginRight (C.rem 0.5)
  ".circle" ? do
    sym borderRadius (C.pct 50)
    width (px 25)
    height (px 25)
    display inlineBlock
    backgroundColor $ parse nord8
  ".bigCircle" ? do
    width (px 50)
    height (px 50)


heroSection :: Html ()
heroSection = div_ [class_ "hero"] $ do
      div_ [class_ "circle bigCircle"] ""
      div_ [class_ "spacing"] $ mapM_ (\_ -> div_ [class_ "circle"] "") [1..3]
      h1_ "Hi, My name is Thanawat!"
      p_ "I'm a student at UCSB studying Computer Science. I have a passion for functional programming and learning new things. My favorite languages are Haskell and Typescript. " -- TODO come up with a good quote

postSheet :: Css
postSheet = ".posts" ?
  textAlign center


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

index :: [Post] -> String
index posts = TZ.unpack $ renderText $ baseTemplate "" $ do
  navBar
  heroSection
  postList posts

-- For rendering individual posts
post :: Html () -> String
post orgHtml = TZ.unpack $ renderText $ baseTemplate "post" orgHtml
