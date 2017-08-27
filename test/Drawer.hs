{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Drawer where

import           Data.OrgMode.Parse
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserDrawerTests :: TestTree
parserDrawerTests = testGroup "Attoparsec PropertyDrawer"
    [ testCase "Parse a :PROPERTY: drawer" $
        testPropertyDrawer

    , testCase "Parse empty :PROPERTY: drawer" $
        (testParser parseProperties ":PROPERTIES:\n:END:\n")

    , testCase "Parse a :LOGBOOK: drawer" $
        testLogbookDrawer

    , testCase "Parse a user-defined drawer" $
        testGenericDrawer
    ]

testPropertyDrawer :: Assertion
testPropertyDrawer =
  testParser parseProperties ":PROPERTIES:\n    :URL: http://someurl.com?query\n :notes: you should be taking them\n:END:\n"

testLogbookDrawer :: Assertion
testLogbookDrawer =
  testParser parseLogbook ":LOGBOOK:\n  CLOCK: [2015-10-05 Mon 17:13] \n CLOCK: [2015-10-05 Mon 17:13]--[2015-10-05 Mon 17:14] =>  0:01\n:END:\n"

testGenericDrawer :: Assertion
testGenericDrawer =
  testParser parseDrawer ":MYDRAWER:\n  whatever I want can go in here, technically... \n:END:\n"
