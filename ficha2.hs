myand::[Bool] -> Bool
myand [] = True
myand(x:xs) = x && myand xs

myor::[Bool] -> Bool
myor[False] = False
myor [] = True
myor(x:xs)= x || myor xs

myconcat:: [[a]] -> [a]
myconcat [] = []
myconcat(x:xs) = x ++ myconcat(xs)