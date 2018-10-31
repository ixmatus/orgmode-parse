module Main where

import           Document
import           Drawer
import           Headline
import           Test.Tasty
import           Timestamps
import           Block.Paragraph
import           Block.List
import           Block.Blocks

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
