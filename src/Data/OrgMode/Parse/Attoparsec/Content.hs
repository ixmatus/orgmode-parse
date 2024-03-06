-----------------------------------------------------------------------------

----------------------------------------------------------------------------

{- |
Module      :  Data.OrgMode.Parse.Attoparsec.Content
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Parsing combinators for org-mode markup and paragraphs.
-}
module Data.OrgMode.Parse.Attoparsec.Content (
    parseContents,
)
where

import Data.Attoparsec.Text (
    Parser,
    eitherP,
    many',
 )

import Data.OrgMode.Parse.Attoparsec.Content.List (parseList)
import Data.OrgMode.Parse.Attoparsec.Content.Paragraph (parseParagraph)
import Data.OrgMode.Parse.Attoparsec.Drawer (parseDrawer)
import Data.OrgMode.Parse.Attoparsec.Util (
    parseLinesTill,
    takeContentBreak,
 )
import Data.OrgMode.Types (Content (..))

-- | Parse the content until reaching a drawer, a list, or a content end.  And include the parsed drawer.
parseContents :: Parser [Content]
parseContents = concat <$> many' p
  where
    p :: Parser [Content]
    p = do
        blocks <- parseLinesTill parseParagraph (eitherP takeContentBreak (parseDrawer <> parseList))
        return $ filter (/= Paragraph []) blocks
