-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Constants
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Constants for some default values.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Constants
(
  keywords
)
where

import           Data.String (IsString)

keywords :: IsString a => [a]
keywords = [ "TODO", "CANCELED", "DONE" ]
