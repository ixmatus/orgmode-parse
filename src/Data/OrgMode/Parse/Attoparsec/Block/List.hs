-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Block.List
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Data.OrgMode.Parse.Attoparsec.Block.List
(
  parseList
)
where

import           Data.Functor
import           Data.Semigroup
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, many1', isHorizontalSpace, char, digit, anyChar)
import qualified Data.Attoparsec.Text           as     Attoparsec.Text
import           Data.OrgMode.Types                    (Item (..), Block (..))
import           Data.OrgMode.Parse.Attoparsec.Util    (parseLinesTill)
import           Data.OrgMode.Parse.Attoparsec.Block.Paragraph    (parseParagraph)


type TokenParser = Parser ([Item] -> Block)

breakout :: forall b. Int -> Parser (Either () b)
breakout n = do
  x <-  Attoparsec.Text.takeWhile isHorizontalSpace
  if Text.compareLength x n == LT
    then fail ""
    else return $ Left ()


takeHorizontalSpace :: Parser ()
takeHorizontalSpace = do
  x <- isHorizontalSpace <$> anyChar
  if x
    then return ()
    else fail ""
takeHorizontalSpaces :: Int -> Parser ()
takeHorizontalSpaces 0 = return ()
takeHorizontalSpaces n = takeHorizontalSpace *> takeHorizontalSpaces (n - 1)

parseItemTokens :: [TokenParser]
parseItemTokens = ordered ++ unordered where
  ordered = [many1' digit $> OrderedList]
  unordered = map parseToken ['*', '-']
  parseToken x = char x $> UnorderedList

data ItemTerm = ItemTerm {
  parseNext :: Parser Item,
  toListBlock :: [Item] -> Block,
  item :: Item
}

-- | Create a Parser to Parse Item
parseItemTermVia :: TokenParser -> Parser ItemTerm
parseItemTermVia p = result where
  result :: Parser ItemTerm
  result = do
    n <- Text.length <$> Attoparsec.Text.takeWhile1 isHorizontalSpace
    ItemTerm (parseItem n)  <$> p <*> (takeHorizontalSpace *> parseItemCore n)
  parseItem ::  Int -> Parser Item
  parseItem n = takeHorizontalSpaces n *> p *> takeHorizontalSpace *> parseItemCore n
  parseItemCore :: Int -> Parser Item
  parseItemCore n =  Item . concat <$> parseLinesTill parseBlocks (breakout n)
  parseBlocks :: Parser [Block]
  parseBlocks = concat <$> Attoparsec.Text.many' (parseLinesTill parseParagraph (Right <$> parseList))


parseItemTerm :: Parser ItemTerm
parseItemTerm = Attoparsec.Text.choice (map parseItemTermVia parseItemTokens)

parseList :: Parser Block
parseList = do
  term <- parseItemTerm
  items <- Attoparsec.Text.many' (parseNext term)
  return $ toListBlock term (item term:items)
