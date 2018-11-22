-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Headline
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode headlines.
----------------------------------------------------------------------------


{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Data.OrgMode.Parse.Attoparsec.Headline
( headlineBelowDepth
, headlineDepth
, headingPriority
, parseStats
, parseTags
, mkTitleMeta
, TitleMeta
)
where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                 as Attoparsec (Parser)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           GHC.Natural                           (Natural)
import           Prelude                               hiding (takeWhile)
import           Text.Printf

import           Data.Functor                          (($>))
import           Data.OrgMode.Parse.Attoparsec.Section
import qualified Data.OrgMode.Parse.Attoparsec.Time    as OrgMode.Time
import           Data.OrgMode.Parse.Attoparsec.Util
import           Data.OrgMode.Types

-- | Intermediate type for parsing titles in a headline after the
-- state keyword and priority have been parsed.
type Tag = Text

newtype TitleMeta = TitleMeta (Text, Maybe Stats, Maybe [Tag])
  deriving (Eq, Show)

-- | Parse an org-mode headline, its metadata, its section-body, and
-- any sub-headlines; please see
-- <http://orgmode.org/worg/dev/org-syntax.html org-syntax>.
--
-- Headline metadata includes a hierarchy level indicated by
-- asterisks, optional todo state keywords, an optional priority
-- level, %-done statistics, and tags; e.g:
--
-- > ** TODO [#B] Polish Poetry Essay [25%] :HOMEWORK:POLISH:WRITING:
--
-- Headlines may contain:
--
-- - A section with Planning and Clock entries
-- - A number of other entities (code blocks, lists)
-- - Unstructured text
-- - Sub-headlines
--
-- @headlineBelowDepth@ takes a list of terms to consider, state
-- keywords, and a minumum hierarchy depth.
--
-- Use a @Depth@ of 0 to parse any headline.
headlineBelowDepth :: [Text]
                  -> Depth
                  -> Attoparsec.Parser Text Headline
headlineBelowDepth stateKeywords d = do
  depth'    <- headlineDepth d <* skipOnlySpace
  stateKey  <- option Nothing (Just <$> parseStateKeyword stateKeywords <* skipOnlySpace)
  priority' <- option Nothing (Just <$> headingPriority <* skipOnlySpace)
  tstamp    <- option Nothing (Just <$> OrgMode.Time.parseTimestamp <* skipOnlySpace)

  -- Parse the title and any metadata within it
  TitleMeta
    ( titleText
    , stats'
    , fromMaybe [] -> tags'
    ) <- parseTitle

  section'      <- parseSection
  subHeadlines' <- option [] $ many' (headlineBelowDepth stateKeywords (d + 1))

  skipSpace
  pure $ Headline
    { depth        = depth'
    , stateKeyword = stateKey
    , priority     = priority'
    , title        = titleText
    , timestamp    = tstamp
    , stats        = stats'
    , tags         = tags'
    , section      = section'
    , subHeadlines = subHeadlines'
    }

-- | Parse the asterisk-indicated headline depth until a space is
-- encountered.
--
-- Constrain it to @Depth@.
headlineDepth :: Depth -> Attoparsec.Parser Text Depth
headlineDepth (Depth d) = takeDepth >>= test
  where
    takeDepth = fromIntegral . Text.length <$> takeWhile1 (== '*')

    test :: Natural -> Attoparsec.Parser Text Depth
    test n | n <= d    = fail (printf "Headline depth of %d cannot be higher than a depth constraint of %d" n d)
           | otherwise = pure (Depth n)

-- | Parse the state indicator.
--
-- > {`TODO` | `DONE` | custom }
--
-- These can be custom so we're parsing additional state identifiers
-- as Text.
parseStateKeyword :: [Text] -> Attoparsec.Parser Text StateKeyword
parseStateKeyword (fmap string -> sk) = StateKeyword <$> choice sk

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail:
--
-- - @[#A]@
-- - @[#B]@
-- - @[#C]@
headingPriority :: Attoparsec.Parser Text Priority
headingPriority = start *> zipChoice <* end
  where
    zipChoice     = choice (zipWith mkPParser "ABC" [A,B,C])
    mkPParser c p = char c $> p
    start         = string "[#"
    end           = char   ']'

-- | Parse the title, optional stats block, and optional tag.
--
-- Stats may be either [m/n] or [n%] and tags are colon-separated, e.g:
-- > :HOMEWORK:POETRY:WRITING:
parseTitle :: Attoparsec.Parser Text TitleMeta
parseTitle =
  mkTitleMeta          <$>
    titleStart         <*>
    optMeta parseStats <*>
    optMeta parseTags  <*>
    -- Parse what's leftover AND till end of line or input; discarding
    -- everything but the leftovers
    leftovers <* (endOfLine <|> endOfInput)
  where
    titleStart = takeTill (\c -> inClass "[:" c || isEndOfLine c)
    leftovers  = option mempty $ takeTill (== '\n')
    optMeta p  = option Nothing (Just <$> p <* skipOnlySpace)

-- | Produce a triple consisting of a stripped start-of-title if there
-- are no leftovers after parsing (otherwise, recombine the two) and
-- the optional stats and tags.
mkTitleMeta :: Text            -- ^ Start of title till the end of line
            -> Maybe Stats     -- ^ Stats, e.g: [33%]
            -> Maybe [Tag]     -- ^ Tags, e.g: :HOMEWORK:CODE:SLEEP:
            -> Text            -- ^ Leftovers (may be empty) of the title
            -> TitleMeta
mkTitleMeta start stats' tags' leftovers =
    TitleMeta (cleanTitle start leftovers, stats', tags')
  where
    cleanTitle t l
      | Text.null leftovers = Text.strip t
      | otherwise           = Text.append t l

-- | Parse a statisticss block, e.g: [33%].
--
-- Accepts either form: "[m/n]" or "[n%]" and there is no restriction
-- on m or n other than that they are integers.
parseStats :: Attoparsec.Parser Text Stats
parseStats = pct <|> frac
  where
    pct  = StatsPct
             <$> (char '[' *> decimal <* string "%]")
    frac = StatsOf
             <$> (char '[' *> decimal)
             <*> (char '/' *> decimal <* char ']')

-- | Parse a colon-separated list of tags.
--
-- > :HOMEWORK:POETRY:WRITING:
parseTags :: Attoparsec.Parser Text [Tag]
parseTags = tags' >>= test
  where
    tags' = char ':' *> takeWhile (/= '\n')
    test t
       | Text.null t        = fail "no data after beginning ':'"
       | Text.last t /= ':' = fail $ Text.unpack $ "expected ':' at end of tag list but got: " `Text.snoc` Text.last t
       | Text.length t < 2  = fail $ Text.unpack $ "not a valid tag set: " <> t
       | otherwise          = pure (Text.splitOn ":" (Text.init t))
