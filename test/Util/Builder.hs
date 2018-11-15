module Util.Builder
  (
    ListBuilder(..),
    ItemBuilder(..),
    ContentBuilder(..)
  )
  where

import           Data.OrgMode.Types                       (Content (..), MarkupText (..), Item (..))
import           Data.Text                                (Text)


class ListBuilder m where
  toL :: ([Item] -> Content) -> m -> Content

instance ListBuilder Text where
  toL l  = toL l . Plain

instance ListBuilder MarkupText where
  toL l = l . (:[]) . Item . (:[]) . Paragraph . (:[]) 

instance ListBuilder Content where
  toL l x = case x of
    UnorderedList _ -> x
    OrderedList _ -> x
    _ -> l [Item [x]]

class ItemBuilder m where
  toI :: m -> Item

instance ItemBuilder Text where
  toI = Item . (:[]) . Paragraph . (:[]) . Plain

class ContentBuilder a where
  toP :: a -> Content
  mark :: ([MarkupText] -> MarkupText) -> a -> Content

instance ContentBuilder Text where
  toP =  toP . Plain
  mark m = mark m . Plain

instance ContentBuilder MarkupText where
  toP = Paragraph . (:[])
  mark m = Paragraph . (:[]) . m . (:[])

instance ContentBuilder Content where
  toP = id
  mark _ = id
