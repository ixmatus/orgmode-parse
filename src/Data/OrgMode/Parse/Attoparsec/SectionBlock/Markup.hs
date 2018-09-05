-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Paragraph.Markup
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards   #-}

module Data.OrgMode.Parse.Attoparsec.SectionBlock.Markup
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
import           Data.OrgMode.Parse.Attoparsec.Util    (takeLinesTill, isHeadLine, takeContentBeforeBlockTill)

data Token = Token { keyChar :: Char, markup :: [MarkupText] -> MarkupText} 

tokens :: [Token]
tokens = [ Token '*' Bold, Token '_' Italic ]

isNotToken :: Char -> Bool
isNotToken c = c /= '*' && c /= '_'

createTokenParser :: Parser [MarkupText] -> Token -> Parser MarkupText
createTokenParser innerParser Token{..}= do 
  _ <- char keyChar
  content <- takeWhile (/= keyChar)
  _ <- char keyChar
  case parseOnly innerParser content of
     Left s -> fail s
     Right a -> return $ markup a

parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  content <- takeWhile isNotToken
  return $ Plain $ refactorLineEnd $ cons c content

refactorLineEnd :: Text -> Text
refactorLineEnd str = fix content where
  textLines = case Text.split isEndOfLine str of 
            [] -> []
            (firstLine : restLines) -> dropWhileEnd isSpace firstLine : map strip restLines
  content = intercalate (Text.pack " ") textLines
  fix s = if isSpace (Text.last str)
           then snoc s ' '
           else s

emptyMarkup :: MarkupText
emptyMarkup = Plain Text.empty
appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement a [] = [a]
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (append nonEmptyText parserFailedText) : xs
appendElement h t
  | h == emptyMarkup = t
  | head t == emptyMarkup = h: tail t
  | otherwise = h:t

parseMarkupContent :: Parser [MarkupText]
parseMarkup :: Parser MarkupText
parseMarkupContent =  foldr appendElement [] <$> manyTill parseMarkup (skipSpace *> endOfInput)
parseMarkup = choice (map (createTokenParser parseMarkupContent) tokens) <> parsePlainText
