-- |

module Page.Components where
import Lucid
import qualified Data.Text as T
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
        <> link_ [rel_ "stylesheet", href_ "/tailwind.css"]
        <> link_ [rel_ "stylesheet", href_ "/js/highlight/styles/nord.min.css"]
        <> script_ [type_ "text/javascript", src_ "/js/ws.js"] ""
        <> script_ [type_ "text/javascript", src_ "/js/highlight/highlight.min.js"] ""
        <> script_ [] "hljs.highlightAll();"
        -- <> link_ [rel_ "stylesheet", href_ "/css/syntax.css"]
  in
  doctype_ <> html_ [lang_ "en"] header <> body_ [class_ $ T.pack bodyClass] body
  
-- This looks terrible on mobile, I need to somehow add a dropdown navBar for mobile.
navBar :: Html ()
navBar = nav_ [class_ "flex p-2 justify-between bg-slate-700 spacing"] $ do
  h1_ [class_ "nord13"] $ a_ [href_ "/"] "Thanawat.xyz"
  div_ [class_ "flex flex-row space-x-4"] $ do
    -- TODO Make these actual links when I actually make the pages
    h2_ [class_ "text-amber-400"] $ a_ [href_ "/posts"] "Blog"
    h2_ [class_ "text-amber-400"] "Resume"
    h2_ [class_ "text-amber-400"] $ do
      a_ [href_ "/about.html"] "About"


-- Takes a list of hex values and generate buttons for them
-- TODO refactor the backgroud color out of the button
-- TODO Make buttons with specific colors

-- TODO add footer..
something = ""
