-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Content.Paragraph
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markup and paragraphs.
----------------------------------------------------------------------------

module Data.OrgMode.Parse.Attoparsec.Content.Paragraph
(
  parseParagraph
)
where

import           Data.Attoparsec.Text                  (Parser)
import           Data.OrgMode.Types                    (Content (..))
import           Data.OrgMode.Parse.Attoparsec.Content.Markup   (parseMarkupContent)

-- | If a chunk of text cannot be parsed as other blocks, parse the chunk of text as a paragraph
parseParagraph :: Parser Content
parseParagraph = Paragraph <$> parseMarkupContent
