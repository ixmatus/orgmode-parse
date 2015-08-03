{-# LANGUAGE OverloadedStrings #-}

module Document where

import           Data.Attoparsec.Text
import           Data.Monoid
import           Data.Text
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as TextIO
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Parse.Types
import           Util

parserSmallDocumentTests :: TestTree
parserSmallDocumentTests = testGroup "Attoparsec Small Document"
  [ testCase "Parse Empty Document"   $ testDocS "" (Document "" [])
  , testCase "Parse No Headlines"     $ testDocS pText (Document pText [])
  , testCase "Parse Heading Sample A" $ testDocS sampleAText sampleAParse
  , testCase "Parse Heading with Planning" $ testDocS samplePText samplePParse
  , testCase "Parse Heading no \n"    $
      testDocS "* T" (Document "" [emptyHeading {title="T"}])
  , testCase "Parse Document from File" $ testDocFile
  ]
  where testDocS s r = expectParse (parseDocument kw) s (Right r)
        testDocF s   = expectParse (parseDocument kw) s (Left "Some failure")
        testDocFile  = do
          doc <- TextIO.readFile "test/test-document.org"
          assertBool "Expected to parse document" . parseSucceeded $ parseOnly (parseDocument kw) doc
        kw           = ["TODO", "CANCELED", "DONE"]
        pText        = "Paragraph text\n.No headline here.\n##--------\n"
        parseSucceeded (Right _) = True
        parseSucceeded (Left _ ) = False


sampleAText :: Text
sampleAText = Text.concat [sampleParagraph,"* Test1", spaces 20,":Hi there:\n"
                          ,"*\n"
                          ," *\n"
                          ,"* Test2    :Two:Tags:\n"
                          ]
sampleAParse :: Document
sampleAParse = Document
               sampleParagraph
               [emptyHeading {title="Test1", tags=["Hi there"]}
               ,emptyHeading {section=emptySection{sectionParagraph=" *\n"}}
               ,emptyHeading {title="Test2", tags=["Two","Tags"]}
               ]

samplePText :: Text
samplePText = Text.concat ["* Test3\n"
                          ,"    SCHEDULED: <2015-06-12 Fri>"
                          ]

samplePParse :: Document
samplePParse = Document
               ""
               [emptyHeading {title="Test3",section=emptySection{sectionPlannings=plns}}
               ]
  where
    plns :: Plannings
    plns = Plns con

    Right con = parseOnly parsePlannings "SCHEDULED: <2015-06-12 Fri>"

emptyHeading :: Heading
emptyHeading = Heading {level = 1
                       ,keyword     = Nothing
                       ,priority    = Nothing
                       ,title       = ""
                       ,stats       = Nothing
                       ,tags        = []
                       ,section     = emptySection
                       ,subHeadings = []
                       }

sampleParagraph :: Text
sampleParagraph = "This is some sample text in a paragraph which may contain * , : , and other special characters.\n\n"

spaces :: Int -> Text
spaces = flip Text.replicate " "

emptySection :: Section
emptySection = Section (Plns mempty) mempty mempty mempty
