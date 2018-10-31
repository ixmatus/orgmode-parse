{-# LANGUAGE OverloadedStrings #-}

module Block.Blocks (
  parserBlocks
                           )
  where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types
import           Data.Text                                     (Text, pack)
import qualified Data.Text                                 as  Text
import           Data.OrgMode.Parse.Attoparsec.Block           (parseBlocks)
import           Util
import           Util.Builder

testDocS :: [Text] -> [Block] -> Assertion
testDocS s expected = expectParse parseBlocks (Text.unlines s) (Right expected)

parserBlocks :: TestTree
parserBlocks = testGroup "Attoparsec orgmode Section Blocks"
  [ testCase "Parses a Single Pargraph" $
      testDocS ["*text *"] [mark Bold (pack "text")],
    testCase "Parses Paragraph and List" $
      testDocS ["*text *", "     * item1 "] [mark Bold (pack "text"), UnorderedList [toI (pack "item1")]],
    testCase "Parses Paragraph and List with blank line" $
      testDocS ["*text *", "   ", "     * item1"] [mark Bold (pack "text"), UnorderedList [toI (pack "item1")]],
    testCase "Parses List and Paragraph" $
      testDocS [ "     * item1", "*text *"] [UnorderedList [toI (pack "item1")], mark Bold (pack "text")],
    testCase "Parses List and Paragraph with blank line" $
      testDocS [ "     * item1", "    ", "*text *"] [UnorderedList [toI (pack "item1")], mark Bold (pack "text")]
  ]
