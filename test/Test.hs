{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Document
import           Headline
import           PropertyDrawer
import           Test.Tasty
import           Timestamps

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
          "OrgMode Parser Tests"
          [ parserHeadlineTests
          , parserPropertyDrawerTests
          , parserTimestampTests
          , parserSmallDocumentTests
          , parserWeekdayTests
          ]
