-- |

module Page.Components where
import Lucid
import Clay as C hiding (type_, title, url)
import qualified Data.Text as T
import Clay.Selector as CS
import Colors


-- I can pull up the metadata into a new type but I'll keep it like this for now
-- TODO allow for ability to add multiple stylesheets here
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
-- This looks terrible on mobile, I need to somehow add a dropdown navBar for mobile.
navBar :: Html ()
navBar = nav_ [class_ ".nav"] $ do
  h1_ [class_ "nord13"] "Thanawat.xyz"
  div_ $ do
    -- TODO Make these actual links when I actually make the pages
    h2_ [class_ "nord11"] "Blog"
    h2_ [class_ "nord12"] "Resume"
    h2_ [class_ "nord15"] "About"


-- Takes a list of hex values and generate buttons for them
-- TODO refactor the backgroud color out of the button
-- TODO Make buttons with specific colors
createButton :: [(T.Text, T.Text)] -> Css
createButton = mapM_ (\color ->
                               CS.selectorFromText  (".button-" <> fst color)  ?
                               do
                                  sym borderRadius (C.px 4)
                                  sym2 padding (C.rem 0.75) (C.rem 0.5)
                                  fontSize (C.rem 1.25)
                                  sym2 margin (C.rem 0) (C.rem 1)
                                  backgroundColor $ parse $ snd color
                                  borderWidth (C.px 0)
                                  fontWeight bold
                                  fontColor $ parse nord4
                                  hover & backgroundColor (darken 0.15 $ parse $ snd color)
                     )

colorsSheet :: Css
colorsSheet = do
  ".nord11" ? color (parse nord11)
  ".nord12" ? color (parse nord12)
  ".nord13" ? color (parse nord13)
  ".nord14" ? color (parse nord14)
  ".nord15" ? color (parse nord15)
