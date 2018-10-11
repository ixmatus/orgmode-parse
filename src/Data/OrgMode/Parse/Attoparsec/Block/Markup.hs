-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Block.Markup
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.OrgMode.Parse.Attoparsec.Block.Markup
(
  parseMarkupContent,
  parsePlainText,
)
where

import           Control.Applicative
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text, cons, append, cons, snoc, intercalate, dropWhileEnd, strip)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, takeWhile, choice, char, anyChar, parseOnly, isEndOfLine, endOfInput, manyTill, skipSpace)
import           Data.OrgMode.Types                    (MarkupText (..))
import           Prelude                        hiding (takeWhile)

-- | I do not know it yet
--
--  TODO: Support LaTeX
data Token = Token { keyChar :: Char, markup :: [MarkupText] -> MarkupText}

-- | A set of definition for markup keyword
tokens :: [Token]
tokens = [ Token '*' Bold, Token '_' Italic ]

-- | For better efficiency suggested by Attoparsec, we will hard code the token filter
isNotToken :: Char -> Bool
isNotToken c = c /= '*' && c /= '_' && c/= '$'

-- | A Naive parser for LaTeX
parseLaTeX :: Parser MarkupText
parseLaTeX = char '$' *> (LaTeX <$> parseL) where
  parseL = do
    content <- takeWhile (/= '$')
    if Text.last content /= '\\'
      then return content
      else  append content <$> parseL

-- | Create a markup parser based on a token
createTokenParser :: Parser [MarkupText] -> Token -> Parser MarkupText
createTokenParser innerParser Token{..}= do
  _ <- char keyChar
  _ <- skipSpace
  content <- takeWhile (/= keyChar)
  _ <- char keyChar
  -- We need another parser passed in to parse the markup inside the markup
  case parseOnly innerParser content of
     Left s -> fail s
     Right a -> return $ markup a

-- | The fallback default if all markup parser fails
parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  content <- takeWhile isNotToken
  return $ Plain $ refactorLineEnd $ cons c content

-- | Take the line break as common space
--
-- 1. spaces before "\n" shall be omitted
-- 2. spaces after "\n" shall be omitted
-- 3. "\n" shall be considered as simple " "
refactorLineEnd :: Text -> Text
refactorLineEnd str = fix content where
  textLines = case Text.split isEndOfLine str of
            [] -> []
            (firstLine : restLines) -> dropWhileEnd isSpace firstLine : map strip restLines
  content = intercalate (Text.pack " ") textLines
  fix s = if isSpace (Text.last str)
           then snoc s ' '
           else s

-- | Normalize to a concise Markup Array after serially running parsers.
--
--  1. Concat the Neighbour Plain Texts
--  2. Remove empty Plain Text
--  3. Remove the Plain spaces at the end
appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement (Plain t) []
  | strip t == "" = []
  | otherwise = [Plain (strip t)]

appendElement a [] = [a]
appendElement (Plain text1) (Plain text2: xs)
 | Text.null text1 && Text.null text2 = xs
 | otherwise = Plain (append text1 text2) : xs
appendElement h t
  | h == Plain Text.empty = t
  | head t == Plain Text.empty = h: tail t
  | otherwise = h:t

-- |Parse the whole text content to an array of Markup Text.
-- This parser will not handle the block stop.  The block stop shall be already handled before passing text with this Parser
parseMarkupContent :: Parser [MarkupText]
parseMarkupContent =  foldr appendElement [] <$> manyTill parseMarkup (skipSpace *> endOfInput) where
  parseMarkup :: Parser MarkupText
  parseMarkup = choice (map (createTokenParser parseMarkupContent) tokens) <> parseLaTeX <> parsePlainText
