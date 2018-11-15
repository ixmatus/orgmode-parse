{-# LANGUAGE OverloadedStrings #-}

module Content.List (
  parserLists
  ) where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types                       (Content (..), Item (..))
import           Data.OrgMode.Parse.Attoparsec.Content.List (parseList)
import qualified Data.Text                                as Text
import           Data.Text                                (pack)
import           Util
import           Util.Builder

parserLists :: TestTree
parserLists = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Simple Item" $
      testDocS ["  * text "]  $ toL UnorderedList (pack "text"),
    testCase "Parses a items with line break" $
      testDocS ["  * item1 ", "       poi"]  $ toL UnorderedList (pack "item1 poi"),
    testCase "Parses multi items" $
      testDocS ["  * item1 ", "  * item2 "]  $ UnorderedList $ map toI [pack "item1", pack "item2"],
    testCase "Parses multi items with child list" $
      testDocS ["  * item1 ", "    * child1", "  * item2 "]  $ UnorderedList 
          [
            Item [toP (pack "item1"), UnorderedList [toI (pack "child1")]],
            toI (pack "item2")
          ]
  ]
  where
    testDocS s  = testDocS' (Text.unlines s)
    testDocS' s expected = expectParse parseList s (Right expected)
