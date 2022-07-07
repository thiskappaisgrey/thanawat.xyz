-- |

module Page.Index where
import Lucid
import Clay as C hiding (type_, title, url)
import Types
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Colors
import Page.Components

  
callButtons :: [(T.Text, T.Text)] -> Html ()
callButtons = do
  div_ [class_ "spacing"] . mapM_ (\(buttonText, color) -> button_ [class_ $ "button-" <> color] $ toHtml buttonText)
  -- postList posts
heroSheet :: Css
heroSheet = ".hero" ? do
  textAlign center
  lineHeight (C.rem 1.5)

  width (pct 33)
  marginLeft auto -- margin auto centers things
  marginRight auto
  star ? do
    marginBottom (C.rem 1.5)
  h1 ? do
    fontColor $ parse nord13
  p ? do
    fontSize (C.rem 1.3)
  ".spacing" |> star ? do
    marginLeft (C.rem 0.8)
    marginRight (C.rem 0.8)
  ".circle" ? do
    sym borderRadius (C.pct 50)
    width (px 25)
    height (px 25)
    display inlineBlock
    backgroundColor $ parse nord8
  ".bigCircle" ? do
    width (C.rem 15)
    height (C.rem 15)


heroSection :: Html ()
heroSection = div_ [class_ "hero"] $ do
      -- div_ [class_ "circle bigCircle"] ""
      img_ [src_ "/images/me_square.jpeg", class_ "bigCircle circle"]
      div_ [class_ "spacing"] $ mapM_ (\_ -> div_ [class_ "circle"] "") [1..3] -- TODO add social media links..
      h1_ "Hi, My name is Thanawat!"
      p_ "I'm a student at UCSB studying Computer Science. \
         \I have a passion for functional programming and learning new things. My favorite languages are Haskell and Typescript." -- TODO come up with a good quote
      callButtons [("My Blog", "nord11"), ("My Resume", "nord12"), ("About Me", "nord15")] -- TODO will refactor later
index :: [Post] -> Html ()
index posts = heroSection
