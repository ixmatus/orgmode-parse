-----------------------------------------------------------------------------
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Data.OrgMode.Parse.Attoparsec.Drawer.Generic
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Parsing combinators for org-mode section generic drawers.
-}
module Data.OrgMode.Parse.Attoparsec.Drawer.Generic (
    parseDrawer,
    parseDrawerDelim,
    drawerEnd,
)
where

import Data.Attoparsec.Text (
    Parser,
    asciiCI,
    char,
    manyTill,
    skipSpace,
    takeWhile1,
    (<?>),
 )
import Data.Text (Text)

import qualified Data.OrgMode.Parse.Attoparsec.Util as Util
import Data.OrgMode.Types

import qualified Data.Text as Text

{- | Parse a user-defined drawer.

> :MYTEXT:
> whatever I want, can go in here except for headlines and drawers
> :END:
-}
parseDrawer :: Parser Drawer
parseDrawer =
    Drawer
        <$> parseDrawerName
        <*> (Text.unlines <$> manyTill Util.nonHeadline drawerEnd)

{- | Parse a user-defined drawer's name, e.g:

> :DRAWERNAME:
> my text in a drawer
> :END:
-}
parseDrawerName :: Parser Text
parseDrawerName =
    skipSpace
        *> char ':'
        *> takeWhile1 (/= ':')
        <* char ':'
        <* skipSpace

{- | Parse drawer delimiters, e.g the beginning and end of a property
drawer:

> :PROPERTIES:
> :END:
-}
parseDrawerDelim :: Text -> Parser Text
parseDrawerDelim v =
    skipSpace
        *> char ':'
        *> asciiCI v
        <* char ':'
        <* Util.skipOnlySpace

-- | Parse the @:END:@ of a drawer.
drawerEnd :: Parser Text
drawerEnd = parseDrawerDelim "END" <?> "Expected Drawer End :END:"
