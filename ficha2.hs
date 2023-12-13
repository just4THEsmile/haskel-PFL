myand::[Bool] -> Bool
myand [] = True
myand(x:xs) = x && myand xs

myor::[Bool] -> Bool
myor[False] = False
myor [] = True
myor(x:xs)= x || myor xs


-- a operação ++ é para juntar listas
myconcat:: [[a]] -> [a]
myconcat [] = []
myconcat(x:xs) = x ++ myconcat(xs)


-- o _ significa que pode ser qualquer coisa e o : é para juntar elementos a uma lista
myreplicate:: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n-1) x 

--o
myIndex:: [a] -> Int -> a
myIndex [] _ = error "Index out of Bounds"
myIndex (x:_) 0 = x
myIndex (_:xs) n
    | n < 0 = error "Negative Index"
    | otherwise = myIndex xs (n-1)

myelem:: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem n (x:xs)
    | n == x = True
    | n /= x = myelem n xs

myintersperse:: [a] -> [a] -> [a]
myintersperse _ [] =  []
myintersperse a [x] = [x]
myintersperse a (x:xs) = [x] ++ a ++ (myintersperse a xs)

mdc:: Int -> Int -> Int
mdc a b 
    | b == 0 = a
    | b /= 0 = mdc b (a `mod` b)

myinsert:: Ord a=> a -> [a] -> [a]
myinsert num [] = [num]
myinsert num (x:xs)
    | num <= x = num : x  : xs
    | otherwise =  x : myinsert num xs

myisort:: Ord a => [a] -> [a]
myisort [] = []
myisort (x:xs) = myinsert x (myisort xs)


myminimum:: Ord a => [a] -> a
myminimum [] = error "Empty List"
myminimum [x] = x
myminimum (x:xs)
    | x <= head xs = myminimum (x:tail xs)
    | otherwise = myminimum xs


mydelete:: Eq a => a->[a]->[a]
mydelete del [] = []
mydelete del (x:xs)
    | del == x = xs
    | otherwise = x : mydelete del xs

myssort:: Ord a => [a] -> [a]
myssort [] = []
myssort (x:xs) = (min : myssort (mydelete  min (x:xs)))
    where min = myminimum (x:xs) 

expressao = [x^2| x<-[1..100]]

aprox:: Int -> Double
aprox n = 4 * sum [((-1) ** fromIntegral i) / (2 * fromIntegral i + 1) | i <- [0..n]]

aprox':: Int -> Double
aprox' n = sqrt (12*sum[((-1)^ fromIntegral i) / ((fromIntegral i + 1)^2) |i<-[0..n]])

dotprod:: [Float]->[Float]-> Float
dotprod x y = sum (zipWith (*) x y)

divprop:: Integer -> [Integer]
divprop 1 = []
divprop num = [i|i <- [1..num `div` 2], num `mod` i == 0]

perfect:: Integer -> [Integer]
perfect num = [i|i<-[1..num],sum (divprop i) == i]

pitagoricos:: Integer -> [(Integer,Integer,Integer)]
pitagoricos num = [(x,y,z)|x<-[1..num],y<-[1..num],z<-[1..num],(x^2 + y^2) == z^2]

primo:: Integer -> Bool
primo n
    | length (divprop n) == 1 = True
    | otherwise = False

mersennes :: [Integer]
mersennes = [mersenne | n <- [1..30], let mersenne = 2^n - 1, primo mersenne]