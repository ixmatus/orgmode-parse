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
{-# LANGUAGE TemplateHaskell   #-}

module Data.OrgMode.Parse.Attoparsec.Headings
( headingBelowLevel
, headingLevel
, headingPriority
, headingTitle
)
where

import           Control.Applicative      ((*>), (<*), (<|>),(<$>), pure, (<*>))
import           Control.Monad            (when)
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as TP (Parser)
import           Data.Char                (isUpper)
import           Data.Maybe               (fromMaybe)
import           Data.Text                as Text (Text, append, concat,
                                                   length, replicate, null, pack)
import           Prelude                  hiding (concat, null, takeWhile, sequence_)

import           Data.OrgMode.Parse.Types
import Data.OrgMode.Parse.Attoparsec.Time
import Data.OrgMode.Parse.Attoparsec.PropertyDrawer


------------------------------------------------------------------------------
-- | Parse an org-mode heading.
headingBelowLevel :: [Text] -> Int -> TP.Parser Text Heading
headingBelowLevel otherKeywords levelReq = do
    lvl  <- headingLevel levelReq                      <* skipSpace
    td   <- option Nothing
             (Just <$> parseTodoKeyword otherKeywords) <* skipSpace
    pr   <- option Nothing (Just <$> headingPriority)  <* skipSpace
    (tl, s, k) <- takeTitleExtras
    endOfLine
    sect <- parseSection otherKeywords
    subs <- many' (headingBelowLevel otherKeywords (levelReq + 1))
    return $ Heading lvl td pr tl s (fromMaybe [] k) sect subs


-- | Parse the asterisk indicated heading level until a space is
-- reached.
headingLevel :: Int -> TP.Parser Text Int
headingLevel levelReq = do
  stars <- takeWhile1 (== '*')
  let lvl = Text.length stars
  when (lvl < levelReq) (fail "Heading level too high")
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

parseSection :: [Text] -> TP.Parser Text Section
parseSection td = do
  plns  <- parsePlannings
  props <- parseDrawer
  clks  <- many' parseClock
  leftovers <- pack <$>
               manyTill anyChar (headingBelowLevel td 0)
  return (Section plns props clks leftovers)

-- | Parse the state indicator {`TODO` | `DONE` | otherTodoKeywords }.
--
-- These can be custom so we're parsing additional state
-- identifiers as Text
parseTodoKeyword :: [Text] -> TP.Parser Text TodoKeyword
parseTodoKeyword otherKeywords =
    choice ([string "TODO" *> pure TODO
            ,string "DONE" *> pure DONE
            ] ++
            map (\k -> OtherKeyword <$> string k) otherKeywords)


-- This function tries to parse a title with a keyword and if it fails
-- it then attempts to parse everything till the end of the line.
headingTitle :: TP.Parser Text (Text, Maybe [Tag])
headingTitle = takeTitleKeys <|> takeTitleEnd

takeTitleKeys :: TP.Parser Text (Text, Maybe [Tag])
takeTitleKeys = (,) <$> takeTill (== ':') <*> (Just <$> parseTags)

takeTitleExtras :: TP.Parser Text (Text, Maybe Stats, Maybe [Tag])
takeTitleExtras = do
  titleStart <- takeTill (inClass "[:")
  stats      <- option Nothing (Just <$> parseStats)
  tags       <- option Nothing (Just <$> parseTags)
  leftovers  <- takeTill (== '\n')
  return (append titleStart leftovers, stats, tags)

takeTitleEnd :: TP.Parser Text (Text, Maybe [Tag])
takeTitleEnd = do
    t <- takeTill isEndOfLine
    return (t, Nothing)

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

-- -- | Try to parse a title that has keys.
-- --
-- -- This function recurs for every occurrence of ':' and tries to parse
-- -- it as a keyword. If the keyword parser fails we fold the ':' onto
-- -- our title result. If it succeeds then we return the title *and the
-- -- parsed keyword*.
-- takeTitleKeys :: TP.Parser Text (Text, Maybe Keyword)
-- takeTitleKeys = do
--     t  <- takeWhile $ notInClass ":\n\r"
--     cl <- char ':'
--     k  <- headingKeyword'

--     if isJust k
--     then char ':' *> return (t, k)
--     else do
--         (t', k') <- takeTitleKeys
--         return (concat [t, pack [cl], t'], k')

-- -- | Parse a heading keyword.
-- --
-- -- NOTE: this is meant to be used with `takeTitleKeys` since it cannot
-- -- fail and we use it recursively in that function to determine
-- -- whether we are hitting a keyword chunk or not (and saving it if we
-- -- do!).
-- --
-- -- It is not exported because it is not meant to be used outside of
-- -- the `takeTitleKeys` function.
-- headingKeyword' :: TP.Parser Text (Maybe Keyword)
-- headingKeyword' = do
--     key <- takeWhile $ notInClass " :\n\r"
--     if null key
--     then return Nothing
--     else return . Just $ Keyword key

-- -- | Parse a heading keyword.
-- --
-- -- You can use this with `many'` and `catMaybes` to get a list
-- -- keywords:
-- --
-- -- > keys <- many' headingKeyword
-- -- > return $ catMaybes keys
-- headingKeyword :: TP.Parser Text (Maybe Keyword)
-- headingKeyword = do
--     key <- (takeWhile1 $ notInClass ":\n\r") <* char ':'
--     if null key
--     then return Nothing
--     else return . Just $ Keyword key
