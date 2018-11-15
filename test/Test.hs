module Main where

import           Block.Blocks
import           Block.List
import           Block.Paragraph
import           Document
import           Drawer
import           Headline
import           Test.Tasty
import           Timestamps

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
          "OrgMode Parser Tests"
          [ parserHeadlineTests
          , parserDrawerTests
          , parserTimestampTests
          , parserParagraphs
          , parserLists
          , parserBlocks
          , parserWeekdayTests
          , parserSmallDocumentTests
          ]
