-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Content.Markup
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markup and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.OrgMode.Parse.Attoparsec.Content.Markup
( parseMarkupContent
, parsePlainText
)
where

import           Data.Semigroup       ((<>))
#if __GLASGOW_HASKELL__ >= 810
import           Data.Bifoldable      (bifoldMap)
#endif
import           Data.Attoparsec.Text (Parser, anyChar, char, choice, option,
                                       endOfInput, isEndOfLine, manyTill,
                                       parseOnly, skipSpace, takeWhile)
import           Data.Char            (isSpace)
import           Data.Text            (Text, append, cons, dropWhileEnd,
                                       intercalate, snoc, strip, stripEnd)
import           Prelude              hiding (takeWhile)

import           Data.OrgMode.Types   (MarkupText (..))

import qualified Data.Text            as Text

data Token = Token
  { keyChar :: Char
  , markup  :: [MarkupText] -> MarkupText
  }

-- | A set of token definitions for markup keywords
tokens :: [Token]
tokens =
  [ Token '*' Bold
  , Token '_' UnderLine
  , Token '/' Italic
  , Token '+' Strikethrough
  ]

-- | For better efficiency suggested by Attoparsec, we hard code the
-- token filter.
isNotToken :: Char -> Bool
isNotToken c = c `notElem` tokenKeywods
  where
    tokenKeywods = ['$', '=', '~'] ++ map keyChar tokens

-- | A parser for hyper-link markup.
parseHyperLink :: Parser MarkupText
parseHyperLink = do
  _ <- char '['

  link        <- parseLink
  description <- option Nothing (Just <$> parseDescription)

  _ <- char ']'

  pure HyperLink{..}

  where
    parseLink        = parseBracketText
    parseDescription = parseBracketText
    parseBracketText = char '[' *> takeWhile (/= ']') <* char ']'

-- | A Naive parser for LaTeX
parseLaTeX :: Parser MarkupText
parseLaTeX = char '$' *> (LaTeX <$> parseL)
  where
    parseL = do
      content <- takeWhile (/= '$')
      if Text.last content /= '\\'
        then return content
        else  append content <$> parseL

-- | Create Naive Parser for Code and Verbatim marked-up content.
--
-- This cannot serve LaTeX because LaTeX code may include \$ in LaTeX
-- block.
parseVerbatimLike :: Char -> (Text -> MarkupText) -> Parser MarkupText
parseVerbatimLike c m = char c *> (m <$> parseL)
  where
    parseL = takeWhile (/= c) <* char c

parseVerbatim :: Parser MarkupText
parseVerbatim = parseVerbatimLike '=' Verbatim

parseCode :: Parser MarkupText
parseCode = parseVerbatimLike '~' Code

-- | Create a markup parser based on a token.
createTokenParser :: Parser [MarkupText] -> Token -> Parser MarkupText
createTokenParser innerParser Token{..} = do
  -- Spaces just after the spaces
  _ <- char keyChar <* skipSpace
  content <- takeWhile (/= keyChar)
  _ <- char keyChar
  -- We need another parser passed in to parse the markup inside the markup
#if __GLASGOW_HASKELL__ >= 810
  bifoldMap fail (return . markup) (parseOnly innerParser content)
#else
  case parseOnly innerParser content of
    Left s  -> fail s
    Right a -> return (markup a)
#endif

-- | The fallback default if all markup parsers fail.
parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  -- Append the first char and then refactor all spaces at line end
  -- and line beginning
  content <- adaptSpace . cons c <$> takeWhile isNotToken
  return $ Plain content

-- | Take the line break as common space.
--
-- 1. spaces before "\n" shall be omitted
-- 2. spaces after "\n" shall be omitted
-- 3. "\n" shall be considered as simple " "
adaptSpace :: Text -> Text
adaptSpace str = fix content
  where
    textLines =
      case Text.split isEndOfLine str of
        [] -> []
        (firstLine : restLines) -> dropWhileEnd isSpace firstLine : map strip restLines

    content = intercalate (Text.pack " ") textLines

    fix s | isSpace (Text.last str) = snoc s ' '
          | otherwise               = s

-- | Normalize to a concise Markup Array after serially running
-- parsers.
--
--  1. Concat the Neighbour Plain Texts
--  2. Remove empty Plain Text
--  3. Remove the Plain spaces at the end
appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement (Plain t) []
  -- Remove the spaces by the end of paragraph or of markup
  | strip t == ""
  = []
  | otherwise
  = [Plain (stripEnd t)]

appendElement a [] = [a]
appendElement (Plain text1) (Plain text2: xs)
  | Text.null text1 && Text.null text2
  = xs
  | otherwise
  = Plain (append text1 text2) : xs
appendElement h t
  | h == Plain Text.empty
  = t
  | head t == Plain Text.empty
  = h: tail t
  | otherwise
  = h:t

-- | Parse the whole text content to an array of Markup Text.
--
-- This parser will not handle the block stop. The block stop shall be
-- already handled before passing text with this Parser
parseMarkupContent :: Parser [MarkupText]
parseMarkupContent = foldr appendElement [] <$> manyTill parseMarkup (skipSpace *> endOfInput)
  where
    parseMarkup :: Parser MarkupText
    parseMarkup =
      choice (map (createTokenParser parseMarkupContent) tokens) <>
      choice [ parseHyperLink, parseLaTeX, parseVerbatim, parseCode, parsePlainText ]
