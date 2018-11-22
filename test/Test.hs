module Main where

import           Content.Contents
import           Content.List
import           Content.Paragraph
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
          , parserContents
          , parserWeekdayTests
          , parserSmallDocumentTests
          ]
