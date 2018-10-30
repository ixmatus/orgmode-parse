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

import           Control.Monad                         (guard)
import           Data.Functor                          (($>))
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, many1', isHorizontalSpace, char, digit)
import qualified Data.Attoparsec.Text           as     Attoparsec.Text
import           Data.OrgMode.Types                    (Item (..), Block (..))
import           Data.OrgMode.Parse.Attoparsec.Util    (parseLinesTill, parseLinesContextuallyTill, takeBlockBreak)
import           Data.OrgMode.Parse.Attoparsec.Block.Paragraph    (parseParagraph)
import           Data.Maybe                            (isNothing)

type TokenParser = Parser ([Item] -> Block)

-- | Parser will success when the parser position is in a new item or break out from the list
--
--  The Bool determines that whether it is in the first line and therefore ignore the preceding space checks
breakout :: forall b. Int -> Bool -> (Bool, Parser (Either () b))
breakout n isInFirstLine = (False, result isInFirstLine)
  where
  -- fail will take the following text in the same line into the Item content
  -- see details in parseLinesTill of Util/ParseLinesTill.hs
  result x= guard (not x) *> do
    z <-  Attoparsec.Text.takeWhile isHorizontalSpace
    -- If not enough space in the new line, then it shall be another item or a new list
    guard (Text.compareLength z n  == LT) $> Left ()

takeHorizontalSpaces :: Int -> Parser ()
takeHorizontalSpaces n = Attoparsec.Text.take n >>= assertSpaces
  where
  assertSpaces t = guard (isNothing (Text.find (not . isHorizontalSpace) t))

parseItemTokens :: [TokenParser]
parseItemTokens = ordered ++ unordered
  where
  ordered = [many1' digit $> OrderedList]
  unordered = map parseToken ['*', '-']
  parseToken x = char x $> UnorderedList

data ItemTerm = ItemTerm
  { parseNext :: Parser Item
  , toListBlock :: [Item] -> Block
  , item :: Item
  }

-- | Create a Parser to Parse Item
parseItemTermVia :: TokenParser -> Parser ItemTerm
parseItemTermVia p = result
  where
  result :: Parser ItemTerm
  result = do
    n <- Text.length <$> Attoparsec.Text.takeWhile1 isHorizontalSpace
    ItemTerm (parseItem n)  <$> p <*> (takeHorizontalSpaces 1*> parseItemCore n)

  parseItem ::  Int -> Parser Item
  parseItem n = takeHorizontalSpaces n *> p *> takeHorizontalSpaces 1*> parseItemCore n

  parseItemCore :: Int -> Parser Item
  parseItemCore n =  Item . concat <$> parseLinesContextuallyTill parseBlocks (breakout (n+1)) True

  parseBlocks :: Parser [Block]
  parseBlocks = concat <$> Attoparsec.Text.many' (parseLinesTill parseParagraph (Attoparsec.Text.eitherP takeBlockBreak parseList))

parseItemTerm :: Parser ItemTerm
parseItemTerm = Attoparsec.Text.choice (map parseItemTermVia parseItemTokens)

parseList :: Parser Block
parseList = do
  term <- parseItemTerm
  items <- Attoparsec.Text.many' (parseNext term)
  return $ toListBlock term (item term:items)
