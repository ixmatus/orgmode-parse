{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Markup
  ( parseMarkup
  , Markup (..)
  )
  where

import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                  as Attoparsec
import           Data.Text                              (Text)

-- | Inline markup elements.
-- https://orgmode.org/manual/Emphasis-and-monospace.html
data Markup
  = Bold Text
  | Italic Text
  | Underline Text
  | Monospace Text
  | Code Text
  | Strike Text
  deriving (Show, Eq)

parseMarkup :: Attoparsec.Parser Text Markup
parseMarkup = choice
  [ parseBold
  , parseItalic
  , parseUnderline
  , parseMonospace
  , parseCode
  , parseStrike
  ]

parseWrap :: (Text -> Markup) -> Char -> Attoparsec.Parser Text Markup
parseWrap t c = do
  _ <- char c
  content <- takeTill (== c)
  _ <- char c
  return $ t content

parseItalic :: Attoparsec.Parser Text Markup
parseItalic = parseWrap Italic '/'

parseBold :: Attoparsec.Parser Text Markup
parseBold = parseWrap Bold '*'

parseUnderline :: Attoparsec.Parser Text Markup
parseUnderline = parseWrap Underline '_'

parseMonospace :: Attoparsec.Parser Text Markup
parseMonospace = parseWrap Monospace '='

parseCode :: Attoparsec.Parser Text Markup
parseCode = parseWrap Code '~'

parseStrike :: Attoparsec.Parser Text Markup
parseStrike = parseWrap Strike '+'
