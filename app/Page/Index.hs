-- |

module Page.Index where
import Lucid
import Types
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Colors
import Page.Components
import Control.Lens (Unwrapped)
import Lucid.Bootstrap (rowFluid_)

import Data.Function ((&))
import Development.Shake.FilePath

callButtons :: [(T.Text, T.Text, T.Text)] -> Html ()
callButtons = do
  div_ [class_ "space-x-10"] . mapM_ (\(buttonText, color, link) -> a_ [class_ $ "rounded-sm p-2 bg-amber-600 hover:bg-amber-900", href_ link] $ toHtml buttonText)
  -- postList posts
socialMediaButtons ::  Html ()
socialMediaButtons = do
  [("github", "https://github.com/thiskappaisgrey")
    , ("linkedin", "https://www.linkedin.com/in/thanawat-techaumnuaiwit-109100184/")
    , ("gitlab", "https://gitlab.com/thiskappaisgrey")
    , ("email", "mailto:thanawat6@protonmail.com")
    ] & div_ [class_ "flex space-x-10"] . mapM_ (\(site, link) -> a_ [href_ link] $ img_ [class_ "w-8 h-8", src_ $ T.pack ("/images/" </> site <.> ".svg")])
  

heroSection :: Html ()
heroSection = div_ [class_ "flex flex-col justify-center h-screen items-center p-8 space-y-5"] $ do
      -- div_ [class_ "circle bigCircle"] ""
      img_ [src_ "/images/me_square.jpeg", class_ "rounded-full w-64 w-64"]
      h1_ [class_ "text-xl text-center"] "Hi, My name is Thanawat!"
      socialMediaButtons
      p_ [class_ "max-w-md"] "I'm a student at UCSB studying Computer Science. \
         \I have a passion for functional programming and learning new things. My favorite languages are Haskell, Emacs Lisp, and Rust." -- TODO come up with a good quote
  -- TODO Add Links
      callButtons [("My Blog", "red-500", "/"), ("My Resume", "green-500", "/"), ("About Me", "blue-500", "/about.html")] -- TODO will refactor later
index :: [Post] -> Html ()
index posts = do
  heroSection
