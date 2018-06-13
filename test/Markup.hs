{-# LANGUAGE OverloadedStrings #-}

module Markup where

import           Data.Attoparsec.Text
import           Data.Text
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as TextIO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.HashMap.Strict

import           Data.OrgMode.Parse.Attoparsec.Markup
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Types
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
