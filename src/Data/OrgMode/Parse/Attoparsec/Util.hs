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
, nonHeadline
)
where

import           Control.Applicative   ((<|>))
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Types (Parser)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Functor          (($>))

-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Parser Text ()
skipOnlySpace = Attoparsec.Text.skipWhile spacePred
  where
    spacePred s = s == ' ' || s == '\t'

-- | Parse a non-heading line of a section.
nonHeadline :: Parser Text Text
nonHeadline = nonHeadline0 <|> nonHeadline1
  where
    nonHeadline0 = Attoparsec.Text.endOfLine $> Text.pack ""
    nonHeadline1 = Text.pack <$> do
      h <- Attoparsec.Text.notChar '*'
      t <- Attoparsec.Text.manyTill Attoparsec.Text.anyChar Attoparsec.Text.endOfLine
      pure (h:t)
