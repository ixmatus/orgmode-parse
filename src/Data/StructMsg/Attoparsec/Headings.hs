-----------------------------------------------------------------------------
-- |
-- Module      :  Data.StructMsg.Attoparsec.Headings
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-list headings.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.StructMsg.Attoparsec.Headings where

import           Control.Applicative   ((<*))
import           Data.Attoparsec.Text  as T
import           Data.Attoparsec.Types as TP (Parser)
import           Data.Char             (isUpper)
import           Data.Text             (Text, null, toUpper)
import           Prelude               hiding (null, takeWhile)

data Heading = Heading
    { level    :: Int
    , priority :: Priority
    , state    :: State
    , title    :: Text
    , keywords :: [Text]
    } deriving (Show, Eq)


data Priority = A | B | C
  deriving (Show, Read, Eq, Ord)

newtype State = State Text
  deriving (Show, Eq)

-- | Parse the asterisk indicated heading level until a space is
-- reached.
headingLevel :: TP.Parser Text Int
headingLevel = return . length =<< manyTill (char '*') space

-- | Parse the priority indicator.
--
-- If anything but these priority indicators are used the parser will
-- fail: `[#A]`, `[#B]`, `[#C]`.
headingPriority :: TP.Parser Text Priority
headingPriority = do
    _     <- char '[' <* char '#'
    lvl <- takeWhile $ inClass "ABC"
    _     <- char ']' <* space

    if null lvl
    then fail "Priority must be one of [#A], [#B], or [#C]"
    else return . read . show $ toUpper lvl

-- | Parse the state indicator {`TODO` | `DONE` | `DOING` }.
--
-- These can be custom so we're parsing the state identifier as Text
-- but wrapped with the State newtype.
headingState :: TP.Parser Text State
headingState = return . State =<< takeWhile isUpper <* space
