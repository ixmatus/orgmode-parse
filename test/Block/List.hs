{-# LANGUAGE OverloadedStrings #-}

module SectionBlock.List where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types                       (Paragraph (..), MarkupText (..), Item (..))
import           Data.OrgMode.Parse.Attoparsec.Block.List (parseList)
import           Data.Text                                (unlines)
import           Data.Either
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Simple Item" $
      testDocS ["  * text "]  $ UnorderedList [(Item . Paragraph) [Plain  "text"]],
    testCase "Parses a items with line break" $
      testDocS ["  * item1 ", "       poi"]  $ UnorderedList [(Item . Paragraph) [Plain  "item1 poi"]],
    testCase "Parses multi items" $
      testDocS ["  * item1 ", "  * item2 "]  $ UnorderedList [(Item . Paragraph) [Plain  "item1", Plain "item2"]]
  ]
  where
    testDocS s  = testDocS' (unlines s)
    testDocS' s expected = expectParse parseList s (Right expected)
    testException s expected = expectParse parseList s (Left expected)

