-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Headings
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-list headings.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Headings
( headingBelowLevel
, headingLevel
, headingPriority
)
where

import           Control.Applicative      ((*>), (<*), (<|>),(<$>), pure, (<*>))
import           Control.Monad            (when, void)
import           Data.Monoid              (mempty)
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as TP (Parser)
import           Data.Maybe               (fromMaybe)
import           Data.Text                as Text (Text, append,
                                                   length, pack, unlines)
import           Prelude                  hiding (concat, null, takeWhile, sequence_, unlines)

import           Data.OrgMode.Parse.Types
import Data.OrgMode.Parse.Attoparsec.Time
import Data.OrgMode.Parse.Attoparsec.PropertyDrawer
import Data.OrgMode.Parse.Attoparsec.Section


-- | Parse an org-mode heading.
headingBelowLevel :: [Text] -> Int -> TP.Parser Text Heading
headingBelowLevel otherKeywords levelReq = do
    lvl  <- headingLevel levelReq                      <* skipSpace
    td   <- option Nothing
             (Just <$> parseTodoKeyword otherKeywords) <* skipSpace
    pr   <- option Nothing (Just <$> headingPriority)  <* skipSpace
    (tl, s, k) <- takeTitleExtras                      <* skipSpace
    sect <- parseSection
    subs <- option [] $ many' (headingBelowLevel otherKeywords (levelReq + 1))
    skipSpace
    return $ Heading lvl td pr tl s (fromMaybe [] k) sect subs


-- | Parse the asterisk indicated heading level until a space is
-- reached.
-- Constrain to levelReq level or its children
headingLevel :: Int -> TP.Parser Text Int
headingLevel levelReq = do
  stars <- takeWhile1 (== '*')
  let lvl = Text.length stars
  when (lvl <= levelReq) (fail "Heading level too high")
  return lvl

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail: `[#A]`, `[#B]`, `[#C]`.
headingPriority :: TP.Parser Text Priority
headingPriority = start
                  *> choice (zipWith mkPParser "ABC" [A,B,C])
                  <* end
  where
    mkPParser c p = char c *> pure p
    start         = string "[#"
    end           = char   ']'

takeTitleExtras :: TP.Parser Text (Text, Maybe Stats, Maybe [Tag])
takeTitleExtras = do
  titleStart <- takeTill (inClass "[:\n")
  s          <- option Nothing (Just <$> parseStats) <* many' (char ' ')
  t          <- option Nothing (Just <$> parseTags)  <* many' (char ' ')
  leftovers  <- option mempty (takeTill (== '\n'))
  void (char '\n')
  return (append titleStart leftovers, s, t)

parseStats :: TP.Parser Text Stats
parseStats = sPct <|> sOf
  where sPct = StatsPct
               <$> (char '[' *> decimal <* string "%]")
        sOf  = StatsOf
               <$> (char '[' *> decimal)
               <*> (char '/' *> decimal <* char ']')

parseTags :: TP.Parser Text [Tag]
parseTags = char ':' *> many1 parseTag
  where parseTag = takeWhile (notInClass "\n\t:") <* char ':'
