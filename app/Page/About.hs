module Page.About where
import Types
import           Data.Org  --                 (OrgDoc, orgFile, OrgFile (OrgFile))
import qualified Data.Map                    as M
import           Data.List  (find)
import           Data.List.NonEmpty (toList)
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
    let secs = (docSections $ orgDoc o)
    p <- findPreamble secs
    i <- findInterests secs
    let abtSecs = docSections $ sectionDoc i
    -- -- let p = findPreamble (docSections $ orgDoc o)
    pure $ About {
                aboutPreamble = orgDocToFile $ sectionDoc p
                , aboutSections = fmap interestsToAboutSection $ abtSecs }
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
