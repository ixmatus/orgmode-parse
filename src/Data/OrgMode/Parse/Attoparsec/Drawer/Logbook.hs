-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Drawer.Logbook
-- Copyright   :  © 2017 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode section logbook drawers.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Drawer.Logbook
(
  parseLogbook
)
where

import Control.Applicative                            ((*>))
import Data.Attoparsec.Text
import Data.Attoparsec.Types                        as Attoparsec
import Data.Text                                       (Text)

import Data.OrgMode.Parse.Attoparsec.Drawer.Generic as Drawer.Generic
import Data.OrgMode.Parse.Attoparsec.Time              (parseClock)
import Data.OrgMode.Types

-- | Parse a @LOGBOOK@ drawer.
--
-- > :LOGBOOK:
-- > CLOCK: [2015-10-05 Mon 17:13]--[2015-10-05 Mon 17:14] =>  0:01
-- > :END:
parseLogbook :: Attoparsec.Parser Text Logbook
parseLogbook = Logbook <$> (drawerBegin *> manyTill parseClock Drawer.Generic.drawerEnd)
  where
    drawerBegin = Drawer.Generic.parseDrawerDelim "LOGBOOK"
