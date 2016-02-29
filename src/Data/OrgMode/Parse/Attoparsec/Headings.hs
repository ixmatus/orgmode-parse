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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.OrgMode.Parse.Attoparsec.Headings
( headingBelowLevel
, headingLevel
, headingPriority
, parseStats
, parseTags
)
where

import           Control.Applicative                   (pure, (*>), (<$>), (<*),
                                                        (<*>), (<|>))
import           Control.Monad                         (liftM5, void)
import           Data.Attoparsec.Text                  as T
import           Data.Attoparsec.Types                 as TP (Parser)
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
import           Data.OrgMode.Parse.Types


-- | Parse an org-mode heading and its contained entities (see <http://orgmode.org/worg/dev/org-syntax.html OrgSyntax>).
--
-- Headers include a hierarchy level indicated by asterisks, optional
-- todo states, priority level, %-done stats, and tags.
--
-- > ** TODO [#B] Polish Poetry Essay [25%] :HOMEWORK:POLISH:WRITING:
--
-- Headings may contain:
--
-- - A section with Planning and Clock entries
-- - A number of other not-yet-implemented entities (code blocks, lists)
-- - Unstructured text
-- - Other heading deeper in the hierarchy
--
-- 'headingBelowLevel' takes a list of terms to consider, state
-- keywords, and a minumum hierarchy depth. Use 0 to parse any
-- heading.
headingBelowLevel :: [Text] -> LevelDepth -> TP.Parser Text Heading
headingBelowLevel stateKeywords depth = do
    lvl  <- headingLevel depth <* skipSpace'
    td   <- option Nothing (Just <$> parseStateKeyword stateKeywords <* skipSpace')
    pr   <- option Nothing (Just <$> headingPriority   <* skipSpace')

    TitleMeta tl stats' (fromMaybe [] -> tags') <- takeTitleExtras

    sect <- parseSection
    subs <- option [] $ many' (headingBelowLevel stateKeywords (depth + 1))

    skipSpace

    return $ Heading lvl td pr tl stats' tags' sect subs

-- | Parse the asterisk indicated heading level until a space is
-- reached.
--
-- Constrain it to LevelDepth or its children.
headingLevel :: LevelDepth -> TP.Parser Text Level
headingLevel (LevelDepth d) = takeLevel >>= test
  where
    takeLevel = Text.length <$> takeWhile1 (== '*')
    test l | l <= d    = fail $ printf "Heading level of %d cannot be higher than depth %d" l d
           | otherwise = return $ Level l

-- | Parse the state indicator.
--
-- > {`TODO` | `DONE` | custom }
--
-- These can be custom so we're parsing additional state identifiers
-- as Text.
parseStateKeyword :: [Text] -> TP.Parser Text StateKeyword
parseStateKeyword (map string -> sk) = StateKeyword <$> choice sk

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail:
--
-- - @[#A]@
-- - @[#B]@
-- - @[#C]@
headingPriority :: TP.Parser Text Priority
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
takeTitleExtras :: TP.Parser Text TitleMeta
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
    optionalMetadata p = option Nothing (Just <$> p <* skipSpace')


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
parseStats :: TP.Parser Text Stats
parseStats = sPct <|> sOf
  where sPct = StatsPct
               <$> (char '[' *> decimal <* string "%]")
        sOf  = StatsOf
               <$> (char '[' *> decimal)
               <*> (char '/' *> decimal <* char ']')

-- | Parse a colon-separated list of Tags
--
-- > :HOMEWORK:POETRY:WRITING:
parseTags :: TP.Parser Text [Tag]
parseTags = tags' >>= test
  where
    tags' = (char ':' *> takeWhile (/= '\n'))
    test t
       | Text.null t = fail "no data after ':'"
       | (Text.last t /= ':' || Text.length t < 2) = fail "Not a valid tag set"
       | otherwise = return (splitOn ":" (Text.init t))

skipSpace' :: TP.Parser Text ()
skipSpace' = void $ takeWhile spacePred
  where
    spacePred s = s == ' ' || s == '\t'
