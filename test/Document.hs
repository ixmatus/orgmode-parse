{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Document where

import           Data.Attoparsec.Text
import           Data.HashMap.Strict.InsOrd             hiding (map)
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Types
import           Data.Text                              hiding (map)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Util
import           Util.Builder

import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as TextIO
import qualified Control.Applicative                    as Applicative

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

  , testCase "Parse Headline with properties and sublist" $
      testDocS sampleP2Text sampleP2Parse

  , testCase "Parse Headline with scheduled and sublist" $
      testDocS sampleP3Text sampleP3Parse

  , testCase "Parse Headline no \n" $
      testDocS "* T" (Document "" [emptyHeadline {title="T"}])

  , testCase "Parse Document from File"
      testDocFile

  , testCase "Parse Document with Subtree List Items"
      testSubtreeListItemDocFile
  ]

  where
    testDocS s r = expectParse parseDocument s (Right r)

    testDocFile  = do
      doc <- TextIO.readFile "test/docs/test-document.org"

      let testDoc = parseOnly parseDocument doc

      assertBool "Expected to parse document" (parseSucceeded testDoc)

    testSubtreeListItemDocFile  = do
      doc <- TextIO.readFile "test/docs/subtree-list-items.org"

      -- let subtreeListItemsDoc = parseOnly (parseDocument []) doc

      -- assertBool "Expected to parse document" (subtreeListItemsDoc == goldenSubtreeListItemDoc)
      expectParse (parseDocumentWithKeywords []) doc  goldenSubtreeListItemDoc

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
               -- Headline shall have space after *
               [emptyHeadline {title="Test1", tags=["Hi there"], section = emptySection {sectionContents= [toP (Bold [])]}}
               ,emptyHeadline {title="Test2", tags=["Two","Tags"]}
               ]

samplePText :: Text
samplePText = Text.concat ["* Test3\n"
                          ,"    SCHEDULED: <2015-06-12 Fri>"
                          ]

samplePParse :: Document
samplePParse = Document "" [ emptyHeadline { title="Test3", section = sect } ]
  where
    sect = emptySection{ sectionPlannings = plannings }
      where
        Right plannings = parseOnly parsePlannings "SCHEDULED: <2015-06-12 Fri>"

sampleP2Text :: Text
sampleP2Text =
    Text.concat ["* Test3_1\n"
                ,"    :PROPERTIES:\n"
                ,"    :CATEGORY: testCategory\n"
                ,"    :END:\n"
                ,"    * One bullet list element\n"
                ,"* Test3_2\n"
                ]

sampleP2Parse :: Document
sampleP2Parse =
    Document "" [ emptyHeadline {
                      title = "Test3_1"
                    , section = emptySection {
                          sectionProperties = Properties (fromList [("CATEGORY", "testCategory")])
                        , sectionContents = [UnorderedList [Item [Paragraph [Plain "One bullet list element"]]]]}}
                , emptyHeadline { title = "Test3_2"}]

sampleP3Text :: Text
sampleP3Text =
    Text.concat ["* Test4_1\n"
                ,"    SCHEDULED: <2004-02-29 Sun 10:20>\n"
                ,"    * One bullet list element\n"
                ,"* Test4_2\n"
                ]

sampleP3Parse :: Document
sampleP3Parse =
    Document "" [ emptyHeadline {
                      title = "Test4_1"
                    , section = emptySection {
                          sectionPlannings = [Planning SCHEDULED curTimestamp]
                        , sectionContents = [UnorderedList [Item [Paragraph [Plain "One bullet list element"]]]]}}
                , emptyHeadline { title = "Test4_2"}]
  where
    curTimestamp =
          Timestamp
            (DateTime
              (YearMonthDay 2004 2 29)
              (Just "Sun")
              (Just (10,20))
              Nothing Nothing
            )
            Active
            Nothing


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
emptySection = Section Nothing mempty mempty mempty mempty mempty

plainParagraphs :: Text -> [Content]
plainParagraphs str = [Paragraph [Plain str]]

goldenSubtreeListItemDoc :: Either String Document
goldenSubtreeListItemDoc =
  Right
    (Document
     { documentText = ""
     , documentHeadlines = [
         Headline
         { depth        = Depth 1
         , stateKeyword = Applicative.empty
         , priority     = Applicative.empty
         , title        = "Header1"
         , timestamp    = Applicative.empty
         , stats        = Applicative.empty
         , tags         = Applicative.empty
         , section      =
             Section
             { sectionTimestamp  = Applicative.empty
             , sectionPlannings  = Applicative.empty
             , sectionClocks     = Applicative.empty
             , sectionProperties = Properties mempty
             , sectionLogbook    = Logbook Applicative.empty
             , sectionContents     = Applicative.empty
             }
         , subHeadlines = [
             Headline
             { depth        = Depth 2
             , stateKeyword = Applicative.empty
             , priority     = Applicative.empty
             , title        = "Header2"
             , timestamp    = Applicative.empty
             , stats        = Applicative.empty
             , tags         = Applicative.empty
             , section      =
                 Section
                 { sectionTimestamp  = Applicative.empty
                 , sectionPlannings  = Applicative.empty
                 , sectionClocks     = Applicative.empty
                 , sectionProperties = Properties mempty
                 , sectionLogbook    = Logbook Applicative.empty
                 , sectionContents     = Applicative.empty
                 }
             , subHeadlines = [
                 Headline
                 { depth        = Depth 3
                 , stateKeyword = Applicative.empty
                 , priority     = Applicative.empty
                 , title        = "Header3"
                 , timestamp    = Applicative.empty
                 , stats        = Applicative.empty
                 , tags         = Applicative.empty
                 , subHeadlines = Applicative.empty
                 , section      =
                     Section
                     { sectionTimestamp  = Applicative.empty
                     , sectionPlannings  = Applicative.empty
                     , sectionClocks     = Applicative.empty
                     , sectionProperties = Properties { unProperties = fromList [("ONE", "two")] }
                     , sectionLogbook    = Logbook Applicative.empty
                     , sectionContents     = [ UnorderedList $ map toI [pack "Item1",  pack "Item2"] ]
                     }
                 }
               ]
             }
         , Headline
           { depth        = Depth 2
           , stateKeyword = Applicative.empty
           , priority     = Applicative.empty
           , title        = "Header4"
           , timestamp    = Applicative.empty
           , stats        = Applicative.empty
           , tags         = Applicative.empty
           , subHeadlines = Applicative.empty
           , section      =
               Section
               { sectionTimestamp  = Applicative.empty
               , sectionPlannings  = Applicative.empty
               , sectionClocks     = Applicative.empty
               , sectionProperties = Properties mempty
               , sectionLogbook    = Logbook Applicative.empty
               , sectionContents   = Applicative.empty
               }
           }
         ]
         }
       ]})
