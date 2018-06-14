{-# LANGUAGE OverloadedStrings #-}

module Markup where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Parse.Attoparsec.Markup
import           Util


parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec bold text"
  [ testCase "Parses bold words" $
      testDocS "*bold*" (Bold "bold")
  , testCase "Parses italic words" $
      testDocS "/italic words/" (Italic "italic words")
  ]
  where
    testDocS s r = expectParse parseMarkup s (Right r)
