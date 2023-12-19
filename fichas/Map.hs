module Map (Map, emptyMap, insertMap, lookupMap) where

data Tree k v = Empty | Node k v (Tree k v) (Tree k v) deriving Show
type Map k v = Tree k v

emptyMap :: Map k v
emptyMap = Empty

insertMap :: Ord k => k -> v -> Map k v -> Map k v
insertMap key value Empty = Node key value Empty Empty
insertMap key value (Node k v left right)
    | key < k   = Node k v (insertMap key value left) right
    | key > k   = Node k v left (insertMap key value right)
    | otherwise = Node key value left right

lookupMap :: Ord k => k -> Map k v -> Maybe v
lookupMap _ Empty = Nothing
lookupMap key (Node k v left right)
    | key < k   = lookupMap key left
    | key > k   = lookupMap key right
    | otherwise = Just v