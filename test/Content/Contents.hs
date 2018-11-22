{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Content.Contents
( parserContents
)
where

import           Data.OrgMode.Parse.Attoparsec.Content (parseContents)
import           Data.OrgMode.Types
import           Data.Text                             (Text)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Util
import           Util.Builder

import qualified Data.Text                             as Text

testDocS :: [Text] -> [Content] -> Assertion
testDocS s expected = expectParse parseContents (Text.unlines s) (Right expected)

parserContents :: TestTree
parserContents = testGroup "Attoparsec orgmode Section Contents"
  [ testCase "Parses a Single Paragraph" $
      testDocS
        ["*text *"]
        [mark Bold ("text" :: Text)]

  , testCase "Parses a HyperLink" $
      testDocS
        [ "[[https://orgmode.org/manual/Link-format.html][The Org Manual: Link format]]" ]
        [ Paragraph
            [ HyperLink "https://orgmode.org/manual/Link-format.html" (Just "The Org Manual: Link format") ]
        ]

  , testCase "Parses an italicised HyperLink" $
      testDocS
        [ "/[[Headline 2][The Org Manual: Link format]]/" ]
        [ Paragraph [
            Italic [
              HyperLink "Headline 2" (Just "The Org Manual: Link format")
            ]
          ]
        ]

  , testCase "Parses an italicised HyperLink in an unordered list" $
      testDocS
        [ "  - /[[Headline 2][The Org Manual: Link format]]/" ]
        [ UnorderedList [
            Item [
              Paragraph [
                Italic [
                  HyperLink "Headline 2" (Just "The Org Manual: Link format")
                ]
              ]
            ]
          ]
        ]

  , testCase "Parses Paragraph and List" $
      testDocS
        ["*text *", "     * item1 "]
        [mark Bold ("text" :: Text), UnorderedList [toI @Text "item1"]]

  , testCase "Parses Paragraph and List with blank line" $
      testDocS
        ["*text *", "   ", "     * item1"]
        [mark Bold ("text" :: Text), UnorderedList [toI @Text "item1"]]

  , testCase "Parses List and Paragraph" $
      testDocS
        [ "     * item1", "*text *"]
        [UnorderedList [toI @Text "item1"], mark Bold ("text" :: Text)]
  , testCase "Parses List and Paragraph with blank line" $
      testDocS
        [ "     * item1", "    ", "*text *"]
        [UnorderedList [toI @Text "item1"], mark Bold ("text" :: Text)]
  ]
