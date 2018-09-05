{-# LANGUAGE OverloadedStrings #-}

module SectionBlock.Paragraph (
  parserParagraphs
                           )
  where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types
import           Data.OrgMode.Parse.Attoparsec.SectionBlock (parseBlockAndDrawer)
import           Data.Either        ()
import           Util


parserParagraphs :: TestTree
parserParagraphs = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Single Markup" $
      testDocS "* text *"  [Bold [Plain  " text "]],
    testCase "Parses a Plain Text (with line break)" $
      testDocS " text \n"    [Plain " text"],
    testCase "Parses a Plain Text (without line break)" $
      testDocS " text "    [Plain " text"],
    testCase "Parses a broken markup with token at start" $
      testDocS "_ text"   [Plain "_ text"],
    testCase "Parses a broken markup Paragraph with token at end" $
      testDocS " text *"   [Plain " text *"],
    testCase "Parses a broken markup Paragraph with token in middle" $
      testDocS " te*xt "   [Plain   " te*xt"],
    testCase "Parses Nested Markup" $
      testDocS "_* text *_"  [Italic [Bold [Plain  " text "]]],
    testCase "Paragraph Parser shall not try to parse markup across lines" $
      testDocS "_* l1p1 \nl2p2 *_"  [Italic [Bold [Plain  " l1p1 l2p2 "]]],
    testCase "Paragraph Parser shall ignore the space before endOfLine (in plain)" $
      testDocS " l1p1 \nl2p2 "  [Plain  " l1p1 l2p2"],
    testCase "Paragraph Parser shall stop at the empty line" $
      testDocS "l1p1 \n\nl2p2 "  [Plain  "l1p1"]
  ]
    where
    testDocS s expected = expectParse parseBlocks s (Right (toSectionBlock expected))
    toSectionBlock :: [MarkupText] -> [SectionBlock]
    toSectionBlock e = [SectionBlock $ Right $ Paragraph e]
    parseDrawer = fail ""
    parseBlocks = fst <$> parseBlockAndDrawer parseDrawer
