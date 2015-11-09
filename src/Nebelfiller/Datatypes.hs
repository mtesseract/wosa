-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Nebelfiller.Datatypes where

import Graphics.UI.Gtk
import qualified Data.Map.Lazy as MapL

---------------------------------
-- Nebelfiller Data Structures --
---------------------------------

-- The Item Data Type. It is simply a pair of words (one term, in two
-- languages, in our situation).
data Item = Item String String
--          | EmptyItem
          deriving (Eq, Read, Show, Ord)

emptyItem :: Item
emptyItem = Item "" ""

itemGet :: Item -> Integer -> String
itemGet (Item a _) 0 = a
itemGet (Item _ b) 1 = b
itemGet item n = error $ "[itemGet] " ++ show item ++ " " ++ show n

-- Our first list consists of *pairs* of items. i.e., two list-one
-- items always belong together. Hence we introduce the ItemPair data
-- type here.
data ItemPair = ItemPair Item Item deriving (Eq, Read, Show, Ord)

emptyItemPair :: ItemPair
emptyItemPair = ItemPair emptyItem emptyItem

-- A list one pair has, using the items of list-two, to be completed
-- to a set of 4 items. Hence, we define the Quadruple data type. This
-- is just a collection of four items (where the first two items are
-- expected to come from list-one and the last two items are expected
-- to come from list-two).
data Wordset = Wordset Item Item Item Item deriving (Eq, Read, Show, Ord)

wordsetItemIndices :: [Integer]
wordsetItemIndices = [0..3]

-- wordsetItem0 (Wordset a _ _ _) = a
-- wordsetItem1 (Wordset _ b _ _) = b
-- wordsetItem2 (Wordset _ _ c _) = c
-- wordsetItem3 (Wordset _ _ _ d) = d

emptyWordset :: Wordset
emptyWordset = Wordset emptyItem emptyItem emptyItem emptyItem

wordsetItem :: Wordset -> Integer -> Item
wordsetItem (Wordset item0 _ _ _) 0 = item0
wordsetItem (Wordset _ item1 _ _) 1 = item1
wordsetItem (Wordset _ _ item2 _) 2 = item2
wordsetItem (Wordset _ _ _ item3) 3 = item3
wordsetItem _ n = error $ "wordSetItem called with n=" ++ show n

data Card = CardA | CardB | CardC | CardD deriving (Eq, Read, Show, Bounded, Enum)

type ListOneCounter = MapL.Map ItemPair (Int, Int)
type ListTwoCounter = MapL.Map Item     (Int, Int)

data BackendCtx =
  BackendCtx { ctxListOne :: [ItemPair]
             , ctxListTwo :: [Item]
             , ctxListOneCounter :: ListOneCounter
             , ctxListTwoCounter :: ListTwoCounter
             , ctxBuilder :: Builder
             , ctxDisableCallbacks :: Bool
             }

-- Convert a list of ItemPairs into a list of Items.
unwrapListOne :: [ItemPair] -> [Item]
unwrapListOne =
  concatMap extractItems
  where extractItems :: ItemPair -> [Item]
        extractItems (ItemPair a b) = [a, b]
