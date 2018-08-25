-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Paragraph
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections #-}

module Data.OrgMode.Parse.Attoparsec.Paragraph
( 
  parseParagraph,
  parsePlainText,
)
where

import           Control.Applicative            
import           Control.Monad                         (when)
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text, cons, append, snoc, intercalate)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, takeWhile, choice, char, anyChar, parseOnly, isEndOfLine, endOfInput, manyTill, (<?>), many', atEnd)
import           Data.OrgMode.Types          (MarkupText (..), Paragraph (..))
import           Prelude                        hiding (takeWhile)
import           Data.OrgMode.Parse.Attoparsec.Util    (takeLinesTill, isHeadLine, takeContentBeforeBlockTill, takeEmptyLine)

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
  return $ Plain $ refactorLineEnd (cons c content) 

refactorLineEnd :: Text -> Text
refactorLineEnd str = fix content where
  content = intercalate (Text.pack " ") (map (Text.dropWhileEnd isSpace) (Text.split isEndOfLine str)) 
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

takeTextAsParagraph :: Text -> Parser Paragraph
takeTextAsParagraph text = 
  case parseOnly parseMarkupContent text of 
    Left s -> fail s
    Right s -> return $ Paragraph s

parseParagraph :: Parser Paragraph
parseParagraph = many' takeEmptyLine *> parseContent where
  parseContent = (dropLastSpaces <$> (isNotAtEndOfInput *> takeContentTillHeadline)) >>= takeTextAsParagraph
  dropLastSpaces = Text.dropWhileEnd isSpace 
  takeContentTillHeadline = takeLinesTill isHeadLine <?> "Not a paragraph line"
  isNotAtEndOfInput = atEnd >>= (`when` fail "")

takeParagraphAndBlock :: Parser s -> Parser (Maybe Paragraph, Maybe s)
takeParagraphAndBlock parseBlock = do
  (content, block) <- takeContentBeforeBlockTill isHeadLine parseBlock
  let paragraphText = Text.dropWhileEnd isSpace content in 
      if Text.null paragraphText 
         then return (Nothing, block)
         else (, block) . Just <$> takeTextAsParagraph content 

parseMarkupContent :: Parser [MarkupText]
parseMarkup :: Parser MarkupText
parseMarkupContent =  foldr appendElement [] <$> manyTill parseMarkup endOfInput  
parseMarkup = choice (map (createTokenParser parseMarkupContent) tokens) <> parsePlainText
