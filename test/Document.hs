{-# LANGUAGE OverloadedStrings #-}

module Document where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid
import Data.Text
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import Data.OrgMode.Parse.Types
import Data.OrgMode.Parse.Attoparsec.Document
import Util

parserSmallDocumentTests :: TestTree
parserSmallDocumentTests = testGroup "Attoparsec Small Document"
  [ testCase "Parse Empty Document"   $ testDocS "" (Document "" [])
  , testCase "Parse No Headlines"     $ testDocS pText (Document pText [])
  , testCase "Parse Heading Sample A" $ testDocS sampleAText sampleAParse
  , testCase "Parse Heading no \n"    $
      testDocS "* T" (Document "" [emptyHeading {title="T"}])
  ]
  where testDocS s r = expectParse (parseDocument kw) s (Right r)
        testDocF s   = expectParse (parseDocument kw) s (Left "Some failure")
        kw           = ["TODO", "CANCELED", "DONE"]
        pText        = "Paragraph text\n.No headline here.\n##--------\n"


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
sampleParagraph = "This is some sample text in a paragraph.\n\n"

spaces :: Int -> Text
spaces = flip Text.replicate " "

emptySection :: Section
emptySection = Section (Plns mempty) mempty mempty mempty
