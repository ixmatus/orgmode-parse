-----------------------------------------------------------------------------
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Data.OrgMode.Parse.Attoparsec.Drawer.Property
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Parsing combinators for org-mode section property drawers.
-}
module Data.OrgMode.Parse.Attoparsec.Drawer.Property (
    parseProperties,
    property,
    PropertyKey,
    PropertyVal,
)
where

import Data.Attoparsec.Text as T
import Data.Attoparsec.Types as Attoparsec
import Data.HashMap.Strict.InsOrd (fromList)
import Data.Text (Text)

import Data.OrgMode.Parse.Attoparsec.Drawer.Generic as Drawer.Generic
import Data.OrgMode.Types

import qualified Data.Text as Text

type PropertyKey = Text
type PropertyVal = Text

{- | Parse a @PROPERTY@ drawer.

> :PROPERTIES:
> :DATE: [2014-12-14 11:00]
> :NOTE: Something really crazy happened today!
> :END:
-}
parseProperties :: Attoparsec.Parser Text Properties
parseProperties = Properties . fromList <$> (drawerBegin *> manyTill property Drawer.Generic.drawerEnd)
  where
    drawerBegin = Drawer.Generic.parseDrawerDelim "PROPERTIES"

{- | Parse a property of a drawer.

Properties *must* be a `:KEY: value` pair, the key can be of any
case and contain any characters except for newlines and colons
(since they delimit the start and end of the key).
-}
property :: Attoparsec.Parser Text (PropertyKey, PropertyVal)
property = (,) <$> parseKey <*> parseVal
  where
    parseKey = skipSpace *> skip (== ':') *> takeWhile1 (/= ':') <* skip (== ':')
    parseVal = Text.strip <$> (skipSpace *> takeValue)
    takeValue = takeWhile1 (not . isEndOfLine)
