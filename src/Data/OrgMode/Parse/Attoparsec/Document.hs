-----------------------------------------------------------------------------

----------------------------------------------------------------------------

{- |
Module      :  Data.OrgMode.Parse.Attoparsec.Document
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Top-level attoparsec parser for org-mode documents.
-}
module Data.OrgMode.Parse.Attoparsec.Document (
    parseDocument,
    parseDocumentWithKeywords,
)
where

import Data.Attoparsec.Text
import Data.Attoparsec.Types as Attoparsec
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.OrgMode.Parse.Attoparsec.Constants as Constants
import Data.OrgMode.Parse.Attoparsec.Headline
import qualified Data.OrgMode.Parse.Attoparsec.Util as Util
import Data.OrgMode.Types

------------------------------------------------------------------------------

{- | Parse a document.

This function uses the following default set of state keywords:
- @TODO@
- @DONE@
- @CANCELLED@

See 'parseDocumentWithKeywords' for a version of the function that
accepts a list of custom state keywords.
-}
parseDocument :: Attoparsec.Parser Text Document
parseDocument = parseDocumentWithKeywords Constants.keywords

-- | Parse a document with a custom list of state keywords.
parseDocumentWithKeywords :: [Text] -> Attoparsec.Parser Text Document
parseDocumentWithKeywords otherKeywords =
    Document
        <$> (Text.unlines <$> many' Util.nonHeadline)
        <*> many' (headlineBelowDepth otherKeywords 0)
