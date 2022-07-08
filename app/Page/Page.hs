-- |

module Page (styleSheet, post, wrapPage) where
import Lucid
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Types
import Clay as C hiding (type_, title, url)
import Colors
import Page.Index       (heroSheet)
import Page.About       (aboutSheet)
import Page.Components
import Data.Org (Todo(TODO))
import Control.Monad (when)
-- TODO Right now, I have 1 style sheet for thing but I might want to separate it later I guess?
styleSheet :: Css
styleSheet = do
  -- get's rid of default styles
  -- fontFace $ do
  --   fontFamily ["FreeMono"] [monospace]
  --   fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeMono.otf"]
  -- fontFace $ do
  --   fontFamily ["FreeMono"] [monospace]
  --   fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeMonoBold.otf"]
  --   fontWeight bold
  -- fontFace $ do
  --   fontFamily ["FreeSans"] [monospace]
  --   fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeSans.otf"]
  -- fontFace $ do
  --   fontFamily ["FreeSans"] [monospace]
  --   fontFaceSrc [FontFaceSrcLocal "/fonts/freefont/FreeSansBold.otf"]
  --   fontWeight bold
  fontFace $ do
    fontFamily ["Rubik"] [sansSerif]
    "src" -: "url('/fonts/Rubik/Rubik-VariableFont_wght.ttf') format('truetype') tech('variations')"
  fontFace $ do
    fontFamily ["RubikMonoOne"] [monospace]
    fontFaceSrc [FontFaceSrcLocal "/fonts/Rubik_Mono_One/RubikMonoOne-Regular.ttf"]
  fontFace $ do
    fontFamily ["Source Sans Pro"] [sansSerif]
    fontFaceSrc [FontFaceSrcLocal "/fonts/Source_Sans_Pro/SourceSansPro-Regular.ttf"]

    -- "src" -: "url('/fonts/Source_Sans_Pro/SourceSansPro-Regular.ttf') format('truetype')"
  fontFace $ do
    fontFamily ["Source Code Pro"] [monospace]
    "src" -: "url('/fonts/Source_Code_Pro/SourceCodePro-VariableFont_wght.ttf') format('truetype') tech('variations')"

  fontFace $ do
    fontFamily ["Fira Code"] [monospace]
    "src" -: "url('/fonts/Fira_Code/FiraCode-VariableFont_wght.ttf') format('truetype') tech('variations')"
    "font-stretch" -: "25% 151%"
    "font-weight" -: "300 700"

  star ? do
    sym C.margin (px 0)
    sym C.padding (px 0)
    C.verticalAlign (px 0)
    fontColor $ parse nord4
    fontFamily ["Rubik"] [ sansSerif ]
    lineHeight (unitless 1.5)
  h1 <> h2 ? do
    fontFamily ["Fira Code"] [ monospace ]
    -- lineHeight (unitless 2.0)
    -- sym2 C.padding (vh 1) 0
    -- "font-stretch" -: "ultra-expanded"
  -- h2 ? do
  --   fontFamily ["Source Code Pro"] [ monospace ]
    -- textTransform uppercase
  a ? do
    color inherit
    textDecoration inherit
  -- TODO make inherit font family??
    fontFamily [] [inherit]

  -- p <> i ? do
  --   fontFamily ["FreeSans"] []
  ".pbody" ? do
    sym2 C.margin 0 (vw 15) -- TODO make padding based on page percentage
    -- textAlign center
    -- h1 ? do
    -- display flex
    -- flexDirection column
    -- alignContent center
    -- justifyContent center


    -- sym C.padding (px 15)
  -- set default body background
  body ? do
    background $ parse nord1
  ".post" ?
    do
      h1 ? do
        textAlign center
        color blue
      h2 ? fontSize (C.rem 2)
  -- TODO I need to refactor this out somehow?
  -- Each page should have it's own sheet
  navSheet
  heroSheet
  postSheet
  aboutSheet
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

wrapPage :: Bool -> Html () -> String
wrapPage n p = TZ.unpack $ renderText $ baseTemplate "" $ do
  when n navBar
  p
-- For rendering individual posts
post :: Html () -> String
post orgHtml = TZ.unpack $ renderText $ baseTemplate "post" orgHtml
