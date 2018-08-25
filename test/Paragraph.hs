{-# LANGUAGE OverloadedStrings #-}
 module Paragraph where
import          Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types (MarkupText (..), Paragraph (..))
import           Data.OrgMode.Parse.Attoparsec.Paragraph (parseParagraph)
import           Data.Text (pack)
import           Data.Either
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Single Markup" $
      testDocS "* text *" $ Paragraph [Bold [(Plain . pack) " text "]],
    testCase "Parses a Plain Text" $
      testDocS " text "   $ Paragraph [(Plain . pack) " text"],
    testCase "Parses a broken markup with token at start" $
      testDocS "_ text "  $ Paragraph [(Plain . pack) "_ text"],
    testCase "Parses a broken markup Paragraph with token at end" $
      testDocS " text *"  $ Paragraph [(Plain . pack) " text *"],
    testCase "Parses a broken markup Paragraph with token in middle" $
      testDocS " te*xt "  $ Paragraph [(Plain . pack) " te*xt"],
    testCase "Parses Nested Markup" $
      testDocS "_* text *_" $ Paragraph [Italic [Bold [(Plain . pack) " text "]]],
    testCase "Parses Multi-lines Markup Paragraph" $
      testDocS "_* l1p1\nl2p2 *_" $ Paragraph [Italic [Bold [(Plain . pack) " l1p1 l2p2 "]]],
    testCase "Parses Multi-lines Markup Paragraph followed a Headline" $
      testDocS "_* l1p1\nl2p2 *_\n" $ Paragraph [Italic [Bold [(Plain . pack) " l1p1 l2p2 "]]],
    testCase "Paragraph Parser shall ignore the space before endOfLine (in markup)" $
      testDocS "_* l1p1 \nl2p2 *_\n" $ Paragraph [Italic [Bold [(Plain . pack) " l1p1 l2p2 "]]],
    testCase "Paragraph Parser shall ignore the space before endOfLine (in plain)" $
      testDocS " l1p1 \nl2p2 " $ Paragraph [(Plain . pack) " l1p1 l2p2"],
    testCase "Paragraph Parser shall stop at the empty line" $
      testDocS "l1p1 \n\nl2p2 " $ Paragraph [(Plain . pack) "l1p1"],
    testCase "Paragraph Parser shall NOT parse the Headline" $
      testException "* text " "Not a paragraph line"
  ]
  where
    testDocS s expected = expectParse parseParagraph (pack s) (Right expected)
    testException s expected = expectParse parseParagraph (pack s) (Left expected)
