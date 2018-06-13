{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Markup
  ( parseMarkup
  , Markup (..)
  )
  where

import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                  as Attoparsec
import           Data.Text                              (Text)

-- | Inline markup elements.
-- https://orgmode.org/manual/Emphasis-and-monospace.html
data Markup
  = Bold Text
  | Italic Text
  -- | Underline
  -- | Monospace
  -- | Code
  -- | Strike
  deriving (Show, Eq)

parseMarkup :: Attoparsec.Parser Text Markup
parseMarkup = choice
  [ parseBold
  , parseItalic
  ]

parseItalic :: Attoparsec.Parser Text Markup
parseItalic = do
  _ <- char '/'
  content <- takeTill (== '/')
  _ <- char '/'
  return $ Italic content

parseBold :: Attoparsec.Parser Text Markup
parseBold = do
  _ <- char '*'
  content <- takeTill (== '*')
  _ <- char '*'
  return $ Bold content
