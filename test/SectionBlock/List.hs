{-# LANGUAGE OverloadedStrings #-}

module SectionBlock.List where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types (MarkupText (..), Item (..))
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Item (parseItems)
import           Data.Either
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Simple Item" $
      testDocS "  * text "  [Item [Plain  "text"]]
  ]
  where
    testDocS s expected = expectParse parseItems s (Right expected)
    testException s expected = expectParse parseItems s (Left expected)

