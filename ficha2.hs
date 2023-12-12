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
myIndex :: [a] -> Int -> a
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