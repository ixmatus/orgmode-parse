module Util.Builder
  (
    ListBuilder(..),
    ItemBuilder(..),
    BlockBuilder(..)
  )
  where

import           Data.OrgMode.Types                       (Block (..), MarkupText (..), Item (..))
import           Data.Text                                (Text)


class ListBuilder m where
  toL :: ([Item] -> Block) -> m -> Block

instance ListBuilder Text where
  toL l  = toL l . Plain

instance ListBuilder MarkupText where
  toL l = l . (:[]) . Item . (:[]) . Paragraph . (:[]) 

instance ListBuilder Block where
  toL l x = case x of
    UnorderedList _ -> x
    OrderedList _ -> x
    _ -> l [Item [x]]

class ItemBuilder m where
  toI :: m -> Item

instance ItemBuilder Text where
  toI = Item . (:[]) . Paragraph . (:[]) . Plain

class BlockBuilder a where
  toP :: a -> Block
  mark :: ([MarkupText] -> MarkupText) -> a -> Block

instance BlockBuilder Text where
  toP =  toP . Plain
  mark m = mark m . Plain

instance BlockBuilder MarkupText where
  toP = Paragraph . (:[])
  mark m = Paragraph . (:[]) . m . (:[])

instance BlockBuilder Block where
  toP = id
  mark _ = id
