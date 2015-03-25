-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.PropertyDrawer
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode entry property drawers.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.OrgMode.Parse.Attoparsec.PropertyDrawer
( drawer
, property
)
where

import           Control.Applicative      ((*>), (<*), (<*>),(<*>),pure)
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as TP
import           Data.HashMap.Strict      (fromList)
import           Data.Text                as Text (Text, strip, pack, unpack)
import           Prelude                  hiding (concat, null, takeWhile)

import           Data.OrgMode.Parse.Types

-- | Parse a property drawer.
--
-- > :PROPERTIES:
-- > :DATE: [2014-12-14 11:00]
-- > :NOTE: Something really crazy happened today!
-- > :END:
drawer :: TP.Parser Text (PropertyDrawer Text Text)
drawer = do
    props <- begin *> manyTill property end
    return . PropertyDrawer $ fromList props
  where
    begin   = ident "PROPERTIES"
    end     = ident "END"
    ident v = skipSpace *> skip (== ':') *>
              asciiCI v                  <*
              skip (== ':') <* skipSpace

-- | Parse a property of a drawer.
--
-- Properties *must* be a `:KEY: value` pair, the key can be of any
-- case and contain any characters except for newlines and colons
-- (since they delimit the start and end of the key).
property :: TP.Parser Text (Text, Text)
property = do
    key <- skipSpace *> skip (== ':') *> takeWhile1 (/= ':') <* skip (== ':')
    val <- skipSpace *> takeValue
    return (key, strip val)
  where
    takeValue = takeWhile1 (not . isEndOfLine)
