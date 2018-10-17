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

import           Control.Monad                         (when)
import           Data.Functor
import           Data.Semigroup
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, many1', isHorizontalSpace, char, digit, anyChar)
import qualified Data.Attoparsec.Text           as     Attoparsec.Text
import           Data.OrgMode.Types                    (Item (..), Block (..))
import           Data.OrgMode.Parse.Attoparsec.Util    (parseLinesTill, parseLinesContextuallyTill, takeBlockBreak)
import           Data.OrgMode.Parse.Attoparsec.Block.Paragraph    (parseParagraph)
import           Data.Maybe                            (isJust)

type TokenParser = Parser ([Item] -> Block)

breakout :: forall b. Int -> Int -> (Int, Parser (Either () b))
breakout n x = (1, result x) where
  result 0 = fail ""
  result _= do
    z <-  Attoparsec.Text.takeWhile isHorizontalSpace
    -- If no enough space in the new line, then it shall be another item or a new list
    if Text.compareLength z n  == LT
      then return $ Left ()
      else fail ""

takeHorizontalSpace :: Parser ()
takeHorizontalSpace = takeHorizontalSpaces 1
takeHorizontalSpaces :: Int -> Parser ()
takeHorizontalSpaces n = do
  t <- Attoparsec.Text.take n
  when (isJust (Text.find (not . isHorizontalSpace) t)) $ fail ""

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
  parseItemCore n =  Item . concat <$> parseLinesContextuallyTill parseBlocks (breakout (n+1)) 0
  parseBlocks :: Parser [Block]
  parseBlocks = concat <$> Attoparsec.Text.many' (parseLinesTill parseParagraph (Attoparsec.Text.eitherP takeBlockBreak parseList))

parseItemTerm :: Parser ItemTerm
parseItemTerm = Attoparsec.Text.choice (map parseItemTermVia parseItemTokens)

parseList :: Parser Block
parseList = do
  term <- parseItemTerm
  items <- Attoparsec.Text.many' (parseNext term)
  return $ toListBlock term (item term:items)
