import Data.List
mapFilter :: (a -> Bool) -> (a -> b) -> [a] -> [b]
mapFilter p f xs = map f (filter p xs)

dec2int::[Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

zipWithRec :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRec _ [] _ = []
zipWithRec _ _ [] = []
zipWithRec f (x:xs) (y:ys) = f x y : zipWithRec f xs ys

myisort:: Ord a => [a] -> [a]
myisort l = foldr (\acc y -> insert acc y) [] l

maximum1:: Ord a => [a] -> a
maximum1 l = foldl1 (\acc x -> if acc > x then acc else x) l

minimum1:: Ord a => [a] -> a
minimum1 l = foldl1 (\acc x -> if acc < x then acc else x) l

myfoldl1 :: (a -> a -> a) -> [a] -> a 
myfoldl1 function [] = error "Empty list"
myfoldl1 function list = foldl (\acc x -> function acc x) (head list) (tail list)

myfoldr1 :: (a -> a -> a) -> [a] -> a 
myfoldr1 function [] = error "Empty list"
myfoldr1 function list = foldr (\x acc -> function acc x) (last list) (init list)

mdc:: Integral a => a -> a -> a
mdc a b = fst(until (\(a, b) -> b == 0) (\(a, b) -> (b, mod a b)) (a, b))

