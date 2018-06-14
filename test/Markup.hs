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
  , testCase "Parses underlined text" $
      testDocS "_underlined stuff_" (Underline "underlined stuff")
  , testCase "Parses monospace text" $
      testDocS "=verbatim=" (Monospace "verbatim")
  , testCase "Parses code" $
      testDocS "~some code~" (Code "some code")
  , testCase "Parses strikethrough" $
      testDocS "+strike this+" (Strike "strike this")
  ]
  where
    testDocS s r = expectParse parseMarkup s (Right r)
