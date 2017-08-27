{-# LANGUAGE OverloadedStrings #-}

module Document where

import           Data.Attoparsec.Text
import           Data.Text
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as TextIO
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Types
import           Util

parserSmallDocumentTests :: TestTree
parserSmallDocumentTests = testGroup "Attoparsec Small Document"
  [ testCase "Parse Empty Document" $
      testDocS "" (Document "" [])

  , testCase "Parse No Headline" $
      testDocS pText (Document pText [])

  , testCase "Parse Headline Sample A" $
      testDocS sampleAText sampleAParse

  , testCase "Parse Headline with Planning" $
      testDocS samplePText samplePParse

  , testCase "Parse Headline no \n" $
      testDocS "* T" (Document "" [emptyHeadline {title="T"}])

  , testCase "Parse Document from File" $
      testDocFile
  ]

  where
    testDocS s r = expectParse (parseDocument kw) s (Right r)
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
               [emptyHeadline {title="Test1", tags=["Hi there"]}
               ,emptyHeadline {section=emptySection{sectionParagraph=" *\n"}}
               ,emptyHeadline {title="Test2", tags=["Two","Tags"]}
               ]

samplePText :: Text
samplePText = Text.concat ["* Test3\n"
                          ,"    SCHEDULED: <2015-06-12 Fri>"
                          ]

samplePParse :: Document
samplePParse = Document
               ""
               [emptyHeadline {title="Test3",section=emptySection{sectionPlannings=plns}}
               ]
  where
    plns :: Plannings
    plns = Plns con

    Right con = parseOnly parsePlannings "SCHEDULED: <2015-06-12 Fri>"

emptyHeadline :: Headline
emptyHeadline =
  Headline
   { depth        = 1
   , stateKeyword = Nothing
   , priority     = Nothing
   , title        = ""
   , stats        = Nothing
   , timestamp    = Nothing
   , tags         = []
   , section      = emptySection
   , subHeadlines = []
   }

sampleParagraph :: Text
sampleParagraph = "This is some sample text in a paragraph which may contain * , : , and other special characters.\n\n"

spaces :: Int -> Text
spaces = flip Text.replicate " "

emptySection :: Section
emptySection = Section Nothing (Plns mempty) mempty mempty mempty mempty mempty
