module Set (Set, emptySet, insertSet, memberSet) where

import Data.List (union, intersect, (\\))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
type Set a = Tree a

emptySet :: Set a
emptySet = Empty

insertSet :: Ord a => a -> Set a -> Set a
insertSet x Empty = Node x Empty Empty
insertSet x (Node y left right)
    | x < y     = Node y (insertSet x left) right
    | x > y     = Node y left (insertSet x right)
    | otherwise = Node y left right

memberSet :: Ord a => a -> Set a -> Bool
memberSet x Empty = False
memberSet x (Node y left right)
    | x < y     = memberSet x left
    | x > y     = memberSet x right
    | otherwise = True

unionSet :: Ord a => Set a -> Set a -> Set a
unionSet Empty t2 = t2
unionSet (Node x left right) t2
    | memberSet x t2 = unionSet left right
    | otherwise      = insertSet x (unionSet left (unionSet right t2))

intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet Empty _ = Empty
intersectSet (Node x left right) t2
    | memberSet x t2 = insertSet x (intersectSet left (intersectSet right t2))
    | otherwise      = intersectSet left right

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet Empty _ = Empty
differenceSet (Node x left right) t2
    | memberSet x t2 = differenceSet left right
    | otherwise      = insertSet x (differenceSet left (differenceSet right t2))