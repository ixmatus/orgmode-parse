{-|
Module      :  Data.OrgMode.Types
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  experimental

Types for the AST of an org-mode document.
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OrgMode.Types.SectionBlock (
  MarkupText     (..),
  SectionBlock   (..),
  List           (..),
  Item           (..),
  Paragraph      (..),
  ) where

import qualified Data.Aeson           as Aeson
import           GHC.Generics
import           Data.Semigroup       (Semigroup)
import           Data.Text            (Text)

data MarkupText = Plain Text | Bold [MarkupText] | Italic [MarkupText] deriving (Show, Eq, Generic)

newtype SectionBlock = SectionBlock (Either List Paragraph) deriving (Show, Eq, Generic, Semigroup)
newtype List = List [Item] deriving (Show, Eq, Generic, Semigroup, Monoid)
newtype Item = Item [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)
newtype Paragraph = Paragraph [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)

instance Aeson.ToJSON MarkupText
instance Aeson.FromJSON MarkupText
instance Aeson.ToJSON Paragraph
instance Aeson.FromJSON Paragraph

instance Aeson.ToJSON List
instance Aeson.FromJSON List
instance Aeson.ToJSON Item
instance Aeson.FromJSON Item

instance Aeson.ToJSON SectionBlock
instance Aeson.FromJSON SectionBlock
