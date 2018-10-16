{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SectionBlock.List where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types                       (Block (..), MarkupText (..), Item (..))
import           Data.OrgMode.Parse.Attoparsec.Block.List (parseList)
import qualified Data.Text                                as Text
import           Data.Text                                (Text, pack)
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Simple Item" $
      testDocS ["  * text "]  $ toL UnorderedList (pack "text"),
    testCase "Parses a items with line break" $
      testDocS ["  * item1 ", "       poi"]  $ toL UnorderedList (pack "item1 poi"),
    testCase "Parses multi items" $
      testDocS ["  * item1 ", "  * item2 "]  $ UnorderedList $ map toI [pack "item1", pack "item2"]
  ]
  where
    testDocS s  = testDocS' (Text.unlines s)
    testDocS' s expected = expectParse parseList s (Right expected)

class ListTest m where
  toL :: ([Item] -> Block) -> m -> Block

instance ListTest Text where
  toL l  = toL l . Plain

instance ListTest MarkupText where
  toL l = l . (:[]) . Item . (:[]) . Paragraph . (:[]) 

instance ListTest Block where
  toL l x = case x of
    UnorderedList _ -> x
    OrderedList _ -> x
    _ -> l [Item [x]]

class ItemTest m where
  toI :: m -> Item

instance ItemTest Text where
  toI = Item . (:[]) . Paragraph . (:[]) . Plain
