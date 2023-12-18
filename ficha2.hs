import Data.Char
import Data.List

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

binom :: Integer -> Integer -> Integer
binom n k = result
    where result = product[1..n]`div`(product[1..k]*product[1..(n-k)])

binom2 :: Integer -> Integer -> Integer
binom2 n k = result
    where result = product [n - k + 1 .. n] `div` product [1 .. k]

pascal:: Integer -> [[Integer]]
pascal n = [[binom i k | k <- [0..i]] | i <- [0..n]]


cifrar :: Int -> String -> String
cifrar shift msg = map (shiftChar shift) msg

shiftChar :: Int -> Char -> Char
shiftChar shift c
    | isLower c = chr $ (ord c - ord 'a' + shift) `mod` 26 + ord 'a'
    | isUpper c = chr $ (ord c - ord 'A' + shift) `mod` 26 + ord 'A'
    | otherwise = c

forte:: String -> Bool
forte password = length password >=8 &&
                any isUpper password &&
                any isLower password &&
                any isDigit password 


mindiv :: Int -> Int
mindiv n = head [i | i <- [2..(floor . sqrt $ fromIntegral n)], n `mod` i == 0]

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : nub1 [y | y <- xs, y /= x]

transpose1 :: [[a]] -> [[a]]
transpose1 [] = []
transpose1 ([]:xss) = transpose1 xss
transpose1 ((x:xs):xss) = (x : [h | (h:_) <- xss]) : transpose1 (xs : [t | (_:t) <- xss])

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)

algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = n `mod` 10 : algarismosRev (n `div` 10)

toBits :: Int -> [Int]
toBits 0 = []
toBits n = toBits (n `div` 2) ++ [n `mod` 2]


fromBits :: [Int] -> Int
fromBits = foldl' (\acc x -> acc * 2 + x) 0


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
           in merge (msort ys) (msort zs)