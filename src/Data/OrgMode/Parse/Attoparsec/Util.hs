{-|
Module      :  Data.OrgMode.Parse.Attoparsec.Util
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities.
-}

module Data.OrgMode.Parse.Attoparsec.Util
( skipOnlySpace
)
where

import           Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Types as Attoparsec (Parser)
import           Data.Text             as Text (Text)

-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Attoparsec.Parser Text ()
skipOnlySpace = Attoparsec.Text.skipWhile spacePred
  where
    spacePred s = s == ' ' || s == '\t'
