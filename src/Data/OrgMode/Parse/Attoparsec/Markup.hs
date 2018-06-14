{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Markup
  ( parseMarkup
  , Markup (..)
  )
  where

import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                  as Attoparsec
import           Data.Text                              (Text)
import           Data.OrgMode.Types

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
