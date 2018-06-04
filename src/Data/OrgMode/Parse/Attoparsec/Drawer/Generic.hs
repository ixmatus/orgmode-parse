-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Drawer.Generic
-- Copyright   :  © 2017 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode section generic drawers.
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Drawer.Generic
( parseDrawer
, parseDrawerDelim
, drawerEnd
)
where

import           Control.Applicative                ((*>), (<*))
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types              as Attoparsec
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Prelude                            hiding (concat, null,
                                                     takeWhile)

import qualified Data.OrgMode.Parse.Attoparsec.Util as Util
import           Data.OrgMode.Types

-- | Parse a user-defined drawer.
--
-- > :MYTEXT:
-- > whatever I want, can go in here except for headlines and drawers
-- > :END:
parseDrawer :: Attoparsec.Parser Text Drawer
parseDrawer =
  Drawer                <$>
    parseDrawerName     <*>
    (Text.unlines <$> manyTill Util.nonHeadline drawerEnd)

-- | Parse a user-defined drawer's name, e.g:
--
-- > :DRAWERNAME:
-- > my text in a drawer
-- > :END:
parseDrawerName :: Attoparsec.Parser Text Text
parseDrawerName =
  skipSpace *> skip (== ':') *>
  takeWhile1 (/= ':')        <*
  skip (== ':') <* skipSpace

-- | Parse drawer delimiters, e.g the beginning and end of a property
-- drawer:
--
-- > :PROPERTIES:
-- > :END:
parseDrawerDelim :: Text -> Attoparsec.Parser Text Text
parseDrawerDelim v =
  skipSpace *> skip (== ':') *>
  asciiCI v                  <*
  skip (== ':') <* Util.skipOnlySpace

-- | Parse the @:END:@ of a drawer.
drawerEnd :: Attoparsec.Parser Text Text
drawerEnd = parseDrawerDelim "END"
