{-|
Module      :  Data.OrgMode.Parse.Attoparsec.Util
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities.
-}


module Data.OrgMode.Parse.Attoparsec.Util
( skipOnlySpace,
  nonHeadline,
  module Data.OrgMode.Parse.Attoparsec.Util.ParseLinesTill
)

where

import           Data.Semigroup
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text  (Parser, takeTill, isEndOfLine, endOfLine, notChar, isHorizontalSpace)
import           Data.Text             (Text, cons)
import qualified Data.Text             as Text
import           Data.Functor          (($>))
import           Data.OrgMode.Parse.Attoparsec.Util.ParseLinesTill

-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Parser ()
skipOnlySpace = Attoparsec.Text.skipWhile isHorizontalSpace

-- | Parse a non-heading line of a section.
nonHeadline :: Parser Text
nonHeadline = nonHeadline0 <> nonHeadline1
  where
    nonHeadline0 = endOfLine $> Text.empty
    nonHeadline1 = cons <$> notChar '*' <*> (takeTill isEndOfLine <* endOfLine)
