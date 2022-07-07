module Page.About (about, aboutSheet, orgDocToData) where
import Types
import           Data.Org  --                 (OrgDoc, orgFile, OrgFile (OrgFile))
import qualified Data.Map                    as M
import           Data.List  (find)
import           Data.List.NonEmpty (toList)
import qualified Data.Org.Lucid as O
import           Lucid
import           Clay
import Page.Components (baseTemplate)
import qualified Data.Text.Lazy as TZ
import qualified Data.Text as T
import Colors
import Data.Bool (bool)
import Control.Monad (zipWithM_)
-- import qualified Data.Foldable      as F
-- the OrgFile with empty metadata can just be turned into content using orgToHTML without having to copy paste code
orgDocToFile :: OrgDoc -> OrgFile
orgDocToFile = OrgFile M.empty

-- I need to some how error if the tag is not found? First, figure out
-- how to break it up into 2 sections first! Then figure out how to
-- build up the page.
data AboutError = PreambleNotFound
                | InterestsNotFound
instance Show AboutError where
  show PreambleNotFound = "Org header with 'preamble' tag not found"
  show InterestsNotFound = "Org header with 'interests' tag not found"
-- In my org file for about, I can find the "headings" by tag. The
-- page has 2 sections: the "preamble" or Introduction and the
-- "Interests" section which will be arranged differently
-- NOTE: A thing with the haskell org-parser is that it expects a newline before each header, so keep that in mind
orgDocToData :: OrgFile -> Either AboutError About
orgDocToData o =
  do
    let secs = docSections $ orgDoc o
    p <- findPreamble secs
    i <- findInterests secs
    let abtSecs = docSections $ sectionDoc i
    -- -- let p = findPreamble (docSections $ orgDoc o)
    pure $ About {
                aboutPreamble = orgDocToFile $ sectionDoc p
                , aboutSections = interestsToAboutSection <$> abtSecs }
  where
    findPreamble :: [Section] -> Either AboutError Section
    findPreamble s =
      let a = find (elem "preamble" . sectionTags) s
      in
        case a of
          Nothing -> Left PreambleNotFound
          Just sec -> Right sec
    findInterests :: [Section] -> Either AboutError Section
    findInterests s =
      let a = find (elem  "interests" . sectionTags) s
      in
        case a of
          Nothing -> Left InterestsNotFound
          Just sec -> Right sec
  -- takes interests with the "interests" tag and turns it into an AboutSection
    interestsToAboutSection :: Section -> AboutSection
    interestsToAboutSection s =
      AboutSection {
        sectionHeader =  mconcat $ fmap prettyWords (toList $ sectionHeading s)
        , sectionContent = orgDocToFile $ sectionDoc s
        , sectionImagePath = M.lookup "image" $ sectionProps s
                   }

-- TODO Add some CSS
preambleSection :: About -> Html ()
preambleSection (About preamble _) = O.body O.defaultStyle preamble

interestToHtml :: Bool -> AboutSection -> Html ()
interestToHtml b (AboutSection h c i ) =
  do
    -- let k = "hello" :: T.Text
    -- TODO How do I switch the order? Maybe by class name?
    div_ [ class_ $ bool "aboutS1" "aboutS2" b ] $ do
        div_ [ class_ "imgSec"]$ do
                div_ [ class_ "square"] ""
        div_ [ class_ "textSec"]$ do
                h2_ $ toHtml h
                O.body O.defaultStyle c
interestsSection :: About -> Html ()
interestsSection (About _ interests) =
  zipWithM_  ($) (cycle [interestToHtml True, interestToHtml False]) interests
-- make about section
-- TODO Responsive design is a bitch
-- TODO Around Width of 850ish I need to change my layout
aboutSheet :: Css
aboutSheet = ".about" ? do
  ".square" ? do
    width (px 400)
    height (px 400)
    backgroundColor $ parse nord8
  ".textSec" ? do
    width (pct 50)
    sym2 padding 0 (pct 5)
  ".imgSec" ? do
    width (pct 50)
    sym2 padding 0 (pct 5)
  ".aboutS1" ? do
    display flex
    flexDirection row
    sym2 margin (pct 5) 0
  ".aboutS2" ? do
    display flex
    flexDirection rowReverse
    justifyContent center
    sym2 margin (pct 5) 0
  ".intro" ? do
    sym2 padding 0 (pct 5)
    sym2 margin (pct 5) 0
    textAlign center
  h1 ? do
    textAlign center

about :: About -> Html ()
-- abstract into a function??
about ab =
  do
  div_ [classes_ ["pbody" , "about"]] $ do
     div_ [class_ "intro"] $ do 
       h1_ "Introduction"
       preambleSection ab
     h1_ "My Interests"
     interestsSection ab
