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
, parseStats
, parseTags
)
where

import           Control.Applicative      ((*>), (<*), (<|>),(<$>), pure, (<*>))
import           Control.Monad            (when, void)
import           Data.Monoid              (mempty)
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as TP (Parser)
import           Data.Maybe               (fromMaybe)
import           Data.Text                as Text (Text, append, init, last, length, null, pack, splitOn, strip, tail)
import           Prelude                  hiding (concat, null, takeWhile, sequence_, unlines)

import           Data.OrgMode.Parse.Types
import           Data.OrgMode.Parse.Attoparsec.Section


-- | Parse an org-mode heading and its contained entities
--   (see orgmode.org/worg/dev/org-syntax.html Header guidance)
--   Headers include a hierarchy level indicated by '*'s,
--   optional Todo-like state, priority level, %-done stats, and tags
--   e.g.:  ** TODO [#B] Polish Poetry Essay  [25%]   :HOMEWORK:POLISH:WRITING:
--
--   Headings contain:
--     * A 'section' with Planning and Clock entries
--     * A number of other not-yet-implemented entities (code blocks, lists)
--     * Unstructured text
--     * Other heading deeper in the hierarchy
--
--   headingBelowLevel takes a list of terms to consider StateKeyword's,
--     and a minumum hierarchy depth. Use 0 to parse any heading
headingBelowLevel :: [Text] -> Int -> TP.Parser Text Heading
headingBelowLevel stateKeywords levelReq = do
    lvl  <- headingLevel levelReq                       <* skipSpace'
    td   <- option Nothing
             (Just <$> parseStateKeyword stateKeywords <* skipSpace')
    pr   <- option Nothing (Just <$> headingPriority   <* skipSpace')
    (tl, s, k) <- takeTitleExtras
    sect <- parseSection
    subs <- option [] $ many' (headingBelowLevel stateKeywords (levelReq + 1))
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


-- | Parse the state indicator {`TODO` | `DONE` | otherTodoKeywords }.
--
-- These can be custom so we're parsing additional state
-- identifiers as Text
parseStateKeyword :: [Text] -> TP.Parser Text StateKeyword
parseStateKeyword stateKeywords = StateKeyword <$>
                                  choice (map string stateKeywords)


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

-- | Parse title, optional Stats block, and optional Tag listToMaybe
--
-- Stats may be either [m/n] or [n%].
-- Tags are colon-separated, e.g.  :HOMEWORK:POETRY:WRITING:
takeTitleExtras :: TP.Parser Text (Text, Maybe Stats, Maybe [Tag])
takeTitleExtras = do
  titleStart <- takeTill (\c -> inClass "[:" c || isEndOfLine c)
  -- skipSpace' doesn't skip newlines. Used here to restrict this
  -- parser to consuming from a single line
  s          <- option Nothing (Just <$> parseStats <* skipSpace')
  t          <- option Nothing (Just <$> parseTags  <* skipSpace')
  leftovers  <- option mempty  (takeTill (== '\n'))
  void (endOfLine <|> endOfInput)
  let titleText
        | null leftovers = strip titleStart
        | otherwise      = append titleStart leftovers
  return (titleText, s, t)


-- | Parse a Stats block.
--
-- Accepts either form: "[m/n]" or "[n%]"
-- There is no restriction on m or n other than that they are integers
parseStats :: TP.Parser Text Stats
parseStats = sPct <|> sOf
  where sPct = StatsPct
               <$> (char '[' *> decimal <* string "%]")
        sOf  = StatsOf
               <$> (char '[' *> decimal)
               <*> (char '/' *> decimal <* char ']')

-- | Parse a colon-separated list of Tags
--
-- e.g. :HOMEWORK:POETRY:WRITING:
parseTags :: TP.Parser Text [Tag]
parseTags = do
  tagsStr <-  (char ':' *> takeWhile (/= '\n'))
  when (Text.last tagsStr /= ':' || Text.length tagsStr < 2) (fail "Not a valid tags set")
  return (splitOn ":" (Text.init  $ tagsStr))

--             (char ':' *> ((takeWhile (\c -> c /= ':' && c /= '\n')) `sepBy` char ':') <* char ':')

skipSpace' :: TP.Parser Text ()
skipSpace' = void (takeWhile (inClass " \t"))
