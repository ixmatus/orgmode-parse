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
( Priority (..)
, State
, Keyword
, Heading (..)
, heading
, headingLevel
, headingPriority
, headingTitle
, headingKeyword'
)
where

import           Control.Applicative   ((*>), (<*), (<|>))
import           Data.Attoparsec.Text  as T
import           Data.Attoparsec.Types as TP (Parser)
import           Data.Char             (isUpper)
import           Data.Maybe            (catMaybes, isJust)
import           Data.Text             (Text, concat, null, pack, toUpper)
import           Prelude               hiding (concat, null, takeWhile)

data Heading = Heading
    { level    :: Int
    , priority :: Maybe Priority
    , state    :: Maybe State
    , title    :: Text
    , keywords :: [Keyword]
    } deriving (Show, Eq)


data Priority = A | B | C
  deriving (Show, Read, Eq, Ord)

newtype State = State Text
  deriving (Show, Eq)

newtype Keyword = Keyword Text
  deriving (Show, Eq, Ord)

-- | Parse an org-mode heading.
heading :: TP.Parser Text Heading
heading = do
    lvl  <- headingLevel
    pr   <- option Nothing headingPriority
    st   <- option Nothing headingState

    (tl, k) <- headingTitle

    keys <- attemptKeys k

    endOfLine

    return $ Heading lvl pr st tl (catMaybes (k ++ keys))

  where
    attemptKeys k | length k > 0 = many' (headingKeyword')
                  | otherwise    = return []

-- | Parse the asterisk indicated heading level until a space is
-- reached.
headingLevel :: TP.Parser Text Int
headingLevel = return . length =<< manyTill (char '*') space

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail: `[#A]`, `[#B]`, `[#C]`.
headingPriority :: TP.Parser Text (Maybe Priority)
headingPriority = do
    _  <- char '[' <* char '#'
    pr <- takeWhile $ inClass "ABC"
    _  <- char ']' <* space

    if null pr
    then fail "Priority must be one of [#A], [#B], or [#C]"
    else return . read . show $ toUpper pr

-- | Parse the state indicator {`TODO` | `DOING` | `DONE`}.
--
-- These can be custom so we're parsing the state identifier as Text
-- but wrapped with the State newtype.
headingState :: TP.Parser Text (Maybe State)
headingState = do
    st <- takeWhile isUpper <* space

    if null st
    then return Nothing
    else return . Just $ State st

takeTitleEnd :: TP.Parser Text (Text, [Maybe Keyword])
takeTitleEnd = do
    t <- takeTill isEndOfLine
    return (t, [])

-- | Parse the title of the heading, stopping at the first keyword we
-- encounter.
headingTitle :: TP.Parser Text (Text, [Maybe Keyword])
headingTitle = takeTitleKeys <|> takeTitleEnd

takeTitleKeys :: TP.Parser Text (Text, [Maybe Keyword])
takeTitleKeys = do
    t  <- takeWhile $ notInClass ":"
    cl <- char ':'
    k  <- headingKeyword

    if isJust k
    then char ':' *> return (t, [k])
    else do
        (t', k') <- takeTitleKeys
        return (concat [t, pack [cl], t'], k')

-- | Parse a heading keyword
headingKeyword :: TP.Parser Text (Maybe Keyword)
headingKeyword = do
    key <- takeWhile $ notInClass " :"
    if null key
    then return Nothing
    else return . Just $ Keyword key

-- | Parse a heading keyword
headingKeyword' :: TP.Parser Text (Maybe Keyword)
headingKeyword' = do
    key <- (takeWhile1 $ notInClass ":\n\r") <* char ':'
    if null key
    then return Nothing
    else return . Just $ Keyword key
