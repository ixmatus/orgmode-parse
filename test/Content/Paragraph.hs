{-# LANGUAGE OverloadedStrings #-}

module Content.Paragraph (
  parserParagraphs
                           )
  where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types
import           Data.Text                                     (Text, pack)
import           Data.OrgMode.Parse.Attoparsec.Content.Paragraph (parseParagraph)
import           Util
import           Util.Builder

testDocS :: (ContentBuilder t) => Text -> t -> Assertion
testDocS s expected = expectParse parseParagraph s (Right (toP expected))

parserParagraphs :: TestTree
parserParagraphs = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Single Markup" $
      testDocS "* text *"  (mark Bold (pack "text")),
    testCase "Parses a Plain Text (with line break)" $
      testDocS " text \n"  (pack " text"),
    testCase "Parses a Plain Text (without line break)" $
      testDocS " text "    (pack " text"),
    testCase "Parses a broken markup with token at start" $
      testDocS "_ text"   (pack "_ text"),
    testCase "Parses a broken markup Paragraph with token at end" $
      testDocS " text *"  (pack " text *"),
    testCase "Parses a broken markup Paragraph with token in middle" $
      testDocS " te*xt "   (pack " te*xt"),
    testCase "Parses Nested Markup" $
      testDocS "_* text *_"  $ mark (UnderLine . (:[]) . Bold) (pack "text"),
    testCase "Paragraph Parser shall not try to parse markup across lines" $
      testDocS "_* l1p1 \nl2p2 *_"  $ mark (UnderLine . (:[]) . Bold) (pack "l1p1 l2p2"),
    testCase "Paragraph Parser shall ignore the space before line end (in plain)" $
      testDocS " l1p1 \nl2p2 "  (pack " l1p1 l2p2"),
    testCase "Paragraph Parser shall ignore the markup inside verbatim" $
      testDocS "= *l1p1 l2p2* ="  $ Paragraph [Verbatim  " *l1p1 l2p2* "]
  ]
