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
( heading
, headingLevel
, headingPriority
, headingTitle
, headingKeyword
)
where

import           Control.Applicative      ((*>), (<*), (<|>))
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as TP (Parser)
import           Data.Char                (isUpper)
import           Data.Maybe               (catMaybes, isJust)
import           Data.Text                as Text (Text, concat, length, null,
                                                   pack)
import           Prelude                  hiding (concat, null, takeWhile)

import           Data.OrgMode.Parse.Types

-- | Parse an org-mode heading.
heading :: TP.Parser Text Heading
heading = do
    lvl  <- headingLevel
    st   <- option Nothing headingState
    pr   <- option Nothing headingPriority

    (tl, k) <- headingTitle

    keys <- attemptKeys k

    endOfLine

    return $ Heading lvl pr st tl (catMaybes (k:keys))

  where
    attemptKeys (Just _) = many' (headingKeyword)
    attemptKeys Nothing  = return []

-- | Parse the asterisk indicated heading level until a space is
-- reached.
headingLevel :: TP.Parser Text Int
headingLevel = return . Text.length =<< takeWhile1 (== '*')

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail: `[#A]`, `[#B]`, `[#C]`.
headingPriority :: TP.Parser Text (Maybe Priority)
headingPriority = do
    pr <- start *> (takeWhile $ inClass "ABC") <* end
    if null pr
    then fail "Priority must be one of [#A], [#B], or [#C]"
    else return . Just $ toPriority pr
  where
    start = char '[' *> char '#'
    end   = char ']' <* space


-- | Parse the state indicator {`TODO` | `DOING` | `DONE`}.
--
-- These can be custom so we're parsing the state identifier as Text
-- but wrapped with the State newtype.
headingState :: TP.Parser Text (Maybe State)
headingState = do
    st <- space *> takeWhile isUpper <* space

    if null st
    then return Nothing
    else return . Just $ State st

-- | Title parser with alternative.
--
-- This function tries to parse a title with a keyword and if it fails
-- it then attempts to parse everything till the end of the line.
headingTitle :: TP.Parser Text (Text, Maybe Keyword)
headingTitle = takeTitleKeys <|> takeTitleEnd

takeTitleEnd :: TP.Parser Text (Text, Maybe Keyword)
takeTitleEnd = do
    t <- takeTill isEndOfLine
    return (t, Nothing)

-- | Try to parse a title that may have keys.
--
-- This function recurs for every occurrence of ':' and tries to parse
-- it as a keyword. If the keyword parser fails we fold the ':' onto
-- our title result. If it succeeds then we return the title *and the
-- parsed keyword*.
takeTitleKeys :: TP.Parser Text (Text, Maybe Keyword)
takeTitleKeys = do
    t  <- takeWhile $ notInClass ":\n\r"
    cl <- char ':'
    k  <- headingKeyword'

    if isJust k
    then char ':' *> return (t, k)
    else do
        (t', k') <- takeTitleKeys
        return (concat [t, pack [cl], t'], k')

-- | Parse a heading keyword.
--
-- NOTE: this is meant to be used with `takeTitleKeys` since it cannot
-- fail and we use it recursively in that function to determine
-- whether we are hitting a keyword chunk or not (and saving it if we
-- do!).
--
-- It is not exported because it is not meant to be used outside of
-- the `takeTitleKeys` function.
headingKeyword' :: TP.Parser Text (Maybe Keyword)
headingKeyword' = do
    key <- takeWhile $ notInClass " :\n\r"
    if null key
    then return Nothing
    else return . Just $ Keyword key

-- | Parse a heading keyword.
--
-- You can use this with `many'` and `catMaybes` to get a list
-- keywords:
--
-- > keys <- many' headingKeyword
-- > return $ catMaybes keys
headingKeyword :: TP.Parser Text (Maybe Keyword)
headingKeyword = do
    key <- (takeWhile1 $ notInClass ":\n\r") <* char ':'
    if null key
    then return Nothing
    else return . Just $ Keyword key
