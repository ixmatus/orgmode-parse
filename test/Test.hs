{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Headings
import           PropertyDrawer
import           Test.Tasty
import           Timestamps

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
          "OrgMode Parser Tests"
          [ parserHeadingTests
          , parserPropertyDrawerTests
          , parserTimestampTests
          ]
