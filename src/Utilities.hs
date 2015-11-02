-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Utilities where

import Data.List
-- import qualified Data.Text as T

----------------------------------------------------------------------
-- Utility Functions --
----------------------------------------------------------------------

nubSort :: (Ord a) => [a] -> [a]
nubSort = nub . sort

-- Compute the cartesian product of two lists.
cartProd :: [a] -> [b] -> [(a,b)]
cartProd listA listB = (,) <$> listA <*> listB

-- Form the intersection list of two lists, i.e. the list of those
-- elements, which are contained in both lists (the list elements are
-- regarded as pairwise distinct).
listIntersection :: (Eq a) => [a] -> [a] -> [a]
listIntersection list0 list1 =
    filter containedInList1 list0
    where containedInList1 x = x `elem` list1

-- Pretty printer for lists.
printList :: (Show a) => [a] -> String
printList [] = "—"
printList list =
  let maxAllowed = 6
  in  if length (take (maxAllowed + 1) list) ==  maxAllowed + 1
         then printList (take maxAllowed list) ++ ", ..."
         else intercalate ", " (map show list)

numberPrefix :: Integer -> Integer -> String
numberPrefix n m = let noWhitespaces = 0
                       padding = length (show n) - length (show m)
                   in replicate (noWhitespaces + padding) ' ' ++ show m ++ ": "
