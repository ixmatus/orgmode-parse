{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Document
import           Drawer
import           Headline
import           Test.Tasty
import           Timestamps
import           Paragraph

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
          "OrgMode Parser Tests"
          [ parserHeadlineTests
          , parserDrawerTests
          , parserTimestampTests
          , parserSmallDocumentTests
          , parserWeekdayTests
          , parserMarkupTests 
          ]
