module Page.About (about, orgDocToData) where
import Types
import           Data.Org  --                 (OrgDoc, orgFile, OrgFile (OrgFile))
import qualified Data.Map                    as M
import           Data.List  (find)
import           Data.List.NonEmpty (toList)
import qualified Data.Org.Lucid as O
import           Lucid
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
                | ResearchNotFound
instance Show AboutError where
  show PreambleNotFound = "Org header with 'preamble' tag not found"
  show InterestsNotFound = "Org header with 'interests' tag not found"
  show ResearchNotFound = "Org header with 'research' tag not found"
-- In my org file for about, I can find the "headings" by tag. The
-- page has 2 sections: the "preamble" or Introduction and the
-- "Interests" section which will be arranged differently
-- NOTE: A thing with the haskell org-parser is that it expects a newline before each header, so keep that in mind

findFromTag ::  T.Text -> e -> [Section] -> Either e Section
findFromTag tag error s =
      let a = find (elem tag . sectionTags) s
      in
        case a of
          Nothing -> Left error
          Just sec -> Right sec
  
  
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
    findPreamble = findFromTag  "preamble" PreambleNotFound
    findInterests :: [Section] -> Either AboutError Section
    findInterests = findFromTag  "interests" InterestsNotFound
    findResearch :: [Section] -> Either AboutError Section
    findResearch = findFromTag  "research" ResearchNotFound
  
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
    div_ [ class_ $ bool "flex flex-row my-8" "flex flex-row-reverse my-8" b ] $ do
        div_ [ class_ "w-1/2 flex items-center justify-center"]$ do
                div_ [ class_ "w-60 h-60 bg-blue-500"] ""
        div_ [ class_ "w-1/2 text-sm"]$ do
                h2_ [class_ "text-lg" ] $ toHtml h
                O.body O.defaultStyle c
interestsSection :: About -> Html ()
interestsSection (About _ interests) =
  zipWithM_  ($) (cycle [interestToHtml True, interestToHtml False]) interests
-- make about section
-- TODO Responsive design is a bitch
-- TODO Around Width of 850ish I need to change my layout
about :: About -> Html ()
-- abstract into a function??
about ab =
  do
   
  div_ [class_ "flex flex-col"] $ div_ [classes_ ["pbody" , "about", "flex", "flex-col", "w-2/3", "place-self-center"]] $ do
     div_  $ do 
       h1_ [class_ "text-xl text-center text-amber-50"] "Introduction"
       preambleSection ab
     h1_ [class_ "text-xl text-center"] "My Interests"
     interestsSection ab
