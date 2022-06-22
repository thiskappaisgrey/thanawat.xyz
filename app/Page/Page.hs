-- |

module Page (styleSheet, index, post) where
import Lucid
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Types
import Clay as C hiding (type_, title, url)
import Colors
import Page.Index
import Page.Components

styleSheet :: Css
styleSheet = do
  -- get's rid of default styles
  fontFace $ do
    fontFamily ["FreeMono"] [monospace]
    fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeMono.otf"]
  fontFace $ do
    fontFamily ["FreeMono"] [monospace]
    fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeMonoBold.otf"]
    fontWeight bold
  fontFace $ do
    fontFamily ["FreeSans"] [monospace]
    fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeSans.otf"]
  fontFace $ do
    fontFamily ["FreeSans"] [monospace]
    fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeSansBold.otf"]
    fontWeight bold
  star ? do
    sym C.margin (px 0)
    sym C.padding (px 0)
    C.verticalAlign (px 0)
    fontColor $ parse nord4
    fontFamily ["FreeMono"] []
  p ? do
    fontFamily ["FreeSans"] []
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
  createButton [("nord11", nord11), ("nord12", nord12), ("nord13", nord13), ("nord14", nord14), ("nord15", nord15)]
  colorsSheet
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


-- For rendering individual posts
post :: Html () -> String
post orgHtml = TZ.unpack $ renderText $ baseTemplate "post" orgHtml
