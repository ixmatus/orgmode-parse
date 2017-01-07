-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Headline
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-list headings.
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.OrgMode.Parse.Attoparsec.Headline
( headingBelowDepth
, headingDepth
, headingPriority
, parseStats
, parseTags
)
where

import           Control.Applicative                   (pure, (*>), (<$>), (<*),
                                                        (<*>), (<|>))
import           Control.Monad                         (liftM5, void)
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                 as Attoparsec (Parser)
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           (mempty)
import           Data.Text                             as Text (Text, append,
                                                                init, last,
                                                                length, null,
                                                                splitOn, strip)
import           Prelude                               hiding (concat, null,
                                                        sequence_, takeWhile,
                                                        unlines)
import           Text.Printf

import           Data.OrgMode.Parse.Attoparsec.Section
import           Data.OrgMode.Parse.Attoparsec.Util
import           Data.OrgMode.Parse.Types


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
-- - A number of other not-yet-implemented entities (code blocks, lists)
-- - Unstructured text
-- - Sub-headlines
--
-- @headingBelowDepth@ takes a list of terms to consider, state
-- keywords, and a minumum hierarchy depth.
--
-- Use a @Depth@ of 0 to parse any headline.
headingBelowDepth :: [Text]
                  -> Depth
                  -> Attoparsec.Parser Text Headline
headingBelowDepth stateKeywords d = do
  dpth <- headingDepth d <* skipOnlySpace
  td   <- option Nothing (Just <$> parseStateKeyword stateKeywords <* skipOnlySpace)
  pr   <- option Nothing (Just <$> headingPriority   <* skipOnlySpace)

  TitleMeta tl stats' (fromMaybe [] -> tags') <- takeTitleExtras

  sect <- parseSection
  subs <- option [] $ many' (headingBelowDepth stateKeywords (d + 1))

  skipSpace

  return $ Headline dpth td pr tl stats' tags' sect subs

-- | Parse the asterisk indicated heading level until a space is
-- reached.
--
-- Constrain it to Depth or its children.
headingDepth :: Depth -> Attoparsec.Parser Text Depth
headingDepth (Depth d) = takeDepth >>= test
  where
    takeDepth = Text.length <$> takeWhile1 (== '*')
    test l | l <= d    = fail $ printf "Headline level of %d cannot be higher than depth %d" l d
           | otherwise = return $ Depth l

-- | Parse the state indicator.
--
-- > {`TODO` | `DONE` | custom }
--
-- These can be custom so we're parsing additional state identifiers
-- as Text.
parseStateKeyword :: [Text] -> Attoparsec.Parser Text StateKeyword
parseStateKeyword (map string -> sk) = StateKeyword <$> choice sk

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
    mkPParser c p = char c *> pure p
    start         = string "[#"
    end           = char   ']'

-- | Parse the title, optional stats block, and optional tag.
--
-- Stats may be either [m/n] or [n%] and tags are colon-separated, e.g:
-- > :HOMEWORK:POETRY:WRITING:
takeTitleExtras :: Attoparsec.Parser Text TitleMeta
takeTitleExtras =
  liftM5 mkTitleMeta
    titleStart
    (optionalMetadata parseStats)
    (optionalMetadata parseTags)
    leftovers
    (void $ endOfLine <|> endOfInput)
  where
    titleStart = takeTill (\c -> inClass "[:" c || isEndOfLine c)
    leftovers  = option mempty $ takeTill (== '\n')
    optionalMetadata p = option Nothing (Just <$> p <* skipOnlySpace)


mkTitleMeta :: Text -> Maybe Stats -> Maybe [Tag] -> Text -> () -> TitleMeta
mkTitleMeta start stats' tags' leftovers _ =
    TitleMeta (transformTitle start leftovers) stats' tags'
  where
    transformTitle t l | null leftovers = strip t
                       | otherwise      = append t l

-- | Parse a stats block.
--
-- Accepts either form: "[m/n]" or "[n%]" and there is no restriction
-- on m or n other than that they are integers.
parseStats :: Attoparsec.Parser Text Stats
parseStats = sPct <|> sOf
  where sPct = StatsPct
               <$> (char '[' *> decimal <* string "%]")
        sOf  = StatsOf
               <$> (char '[' *> decimal)
               <*> (char '/' *> decimal <* char ']')

-- | Parse a colon-separated list of Tags
--
-- > :HOMEWORK:POETRY:WRITING:
parseTags :: Attoparsec.Parser Text [Tag]
parseTags = tags' >>= test
  where
    tags' = (char ':' *> takeWhile (/= '\n'))
    test t
       | Text.null t = fail "no data after ':'"
       | (Text.last t /= ':' || Text.length t < 2) = fail "Not a valid tag set"
       | otherwise = return (splitOn ":" (Text.init t))
