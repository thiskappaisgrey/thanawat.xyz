{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Colors where
import Data.Text (Text)
--NORD COLOR SCHEME------------------------------------------------------------
  -- /*
  -- Base component color of "Polar Night".
  -- Used for texts, backgrounds, carets and structuring characters like curly- and square brackets.
  -- Markup:
  -- <div style="background-color:#2e3440; width=60; height=60"></div>
  -- Styleguide Nord - Polar Night
  -- */
nord0 :: Text
nord0 = "#2e3440"

  -- /*
  -- Lighter shade color of the base component color.
  -- Used as a lighter background color for UI elements like status bars.
  -- Markup:
  -- <div style="background-color:#3b4252; width=60; height=60"></div>
  -- Styleguide Nord - Polar Night
  -- */
nord1 :: Text
nord1 =  "#3b4252"

  -- /*
  -- Lighter shade color of the base component color.
  -- Used as line highlighting in the editor.
  -- In the UI scope it may be used as selection- and highlight color.
  -- Markup:
  -- <div style="background-color:#434c5e; width=60; height=60"></div>
  -- Styleguide Nord - Polar Night
  -- */
nord2 :: Text
nord2 = "#434c5e"

  -- /*
  -- Lighter shade color of the base component color.
  -- Used for comments, invisibles, indent- and wrap guide marker.
  -- In the UI scope used as pseudoclass color for disabled elements.
  -- Markup:
  -- <div style="background-color:#4c566a; width=60; height=60"></div>
  -- Styleguide Nord - Polar Night
  -- */
nord3 :: Text
nord3 = "#4c566a"

  -- /*
  -- Base component color of "Snow Storm".
  -- Main color for text, variables, constants and attributes.
  -- In the UI scope used as semi-light background depending on the theme shading design.
  -- Markup:
  -- <div style="background-color:#d8dee9; width=60; height=60"></div>
  -- Styleguide Nord - Snow Storm
  -- */
nord4 :: Text
nord4 = "#d8dee9"

  -- /*
  -- Lighter shade color of the base component color.
  -- Used as a lighter background color for UI elements like status bars.
  -- Used as semi-light background depending on the theme shading design.
  -- Markup:
  -- <div style="background-color:#e5e9f0; width=60; height=60"></div>
  -- Styleguide Nord - Snow Storm
  -- */
nord5 :: Text
nord5 = "#e5e9f0"

  -- /*
  -- Lighter shade color of the base component color.
  -- Used for punctuations, carets and structuring characters like curly- and square brackets.
  -- In the UI scope used as background, selection- and highlight color depending on the theme shading design.
  -- Markup:
  -- <div style="background-color:#eceff4; width=60; height=60"></div>
  -- Styleguide Nord - Snow Storm
  -- */
nord6 :: Text
nord6 = "#eceff4"

  -- /*
  -- Bluish core color.
  -- Used for classes, types and documentation tags.
  -- Markup:
  -- <div style="background-color:#8fbcbb; width=60; height=60"></div>
  -- Styleguide Nord - Frost
  -- */
nord7 :: Text
nord7 = "#8fbcbb"

  -- /*
  -- Bluish core accent color.
  -- Represents the accent color of the color palette.
  -- Main color for primary UI elements and methods/functions.
  -- Can be used for
  --   - Markup quotes
  --   - Markup link URLs
  -- Markup:
  -- <div style="background-color:#88c0d0; width=60; height=60"></div>
  -- Styleguide Nord - Frost
  -- */
nord8 :: Text
nord8 = "#88c0d0"

  -- /*
  -- Bluish core color.
  -- Used for language-specific syntactic/reserved support characters and keywords, operators, tags, units and
  -- punctuations like (semi)colons,commas and braces.
  -- Markup:
  -- <div style="background-color:#81a1c1; width=60; height=60"></div>
  -- Styleguide Nord - Frost
  -- */
nord9 :: Text
nord9 = "#81a1c1"

  -- /*
  -- Bluish core color.
  -- Used for markup doctypes, import/include/require statements, pre-processor statements and at-rules (`@`).
  -- Markup:
  -- <div style="background-color:#5e81ac; width=60; height=60"></div>
  -- Styleguide Nord - Frost
  -- */
nord10 :: Text
nord10 = "#5e81ac"

  -- /*
  -- Colorful component color.
  -- Used for errors, git/diff deletion and linter marker.
  -- Markup:
  -- <div style="background-color:#bf616a; width=60; height=60"></div>
  -- Styleguide Nord - Aurora
  -- */
nord11 :: Text
nord11 = "#bf616a"

  -- /*
  -- Colorful component color.
  -- Used for annotations.
  -- Markup:
  -- <div style="background-color:#d08770; width=60; height=60"></div>
  -- Styleguide Nord - Aurora
  -- */
nord12 :: Text
nord12 = "#d08770"

  -- /*
  -- Colorful component color.
  -- Used for escape characters, regular expressions and markup entities.
  -- In the UI scope used for warnings and git/diff renamings.
  -- Markup:
  -- <div style="background-color:#ebcb8b; width=60; height=60"></div>
  -- Styleguide Nord - Aurora
  -- */
nord13 :: Text
nord13 = "#ebcb8b"

  -- /*
  -- Colorful component color.
  -- Main color for strings and attribute values.
  -- In the UI scope used for git/diff additions and success visualizations.
  -- Markup:
  -- <div style="background-color:#a3be8c; width=60; height=60"></div>
  -- Styleguide Nord - Aurora
  -- */
nord14 :: Text
nord14 = "#a3be8c"

  -- /*
  -- Colorful component color.
  -- Used for numbers.
  -- Markup:
  -- <div style="background-color:#b48ead; width=60; height=60"></div>
  -- Styleguide Nord - Aurora
  -- */
nord15 :: Text
nord15 = "#b48ead"
