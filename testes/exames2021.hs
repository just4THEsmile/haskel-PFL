maxpos :: [Int] -> Int
maxpos [] = 0
maxpos [x] = x
maxpos (x:xs)
    | x > (head xs) = maxpos (x:(tail xs))
    | otherwise = maxpos (xs)


dups :: [a] ->  [a]
dups [] = []
dups (x:xs) = x:x:(nodups xs)

nodups :: [a] -> [a]
nodups [] = []
nodups (x:xs) = x:(dups xs)

transforma :: String -> String
transforma [] = []
transforma (x:xs)
    | x `elem` ['a','e','i','o','u'] = x:'p':x:(transforma xs)
    | otherwise = x:(transforma xs)


type Vector = [Int]
type Matriz = [[Int]]

transposta:: Matriz -> Matriz
transposta [] = []
transposta m = [ (head x) | x<-m]:transposta [tail x| x <-m,tail x/=[]]

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno v t = (head v)*(head t)+ (prodInterno (tail v) (tail t))

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = prodMatAux m1 (transposta m2)

prodMatAux :: Matriz -> Matriz -> Matriz
prodMatAux [] [] = []
prodMatAux m1 m2 = [[prodInterno v1 v2 | v2 <- m2] | v1 <- m1]

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a left right) = N (alturasAux(N a left right)) (alturas left) (alturas right)

alturasAux :: Arv a -> Int
alturasAux F = 0
alturasAux (N a left right) = 1 + max (alturasAux left) (alturasAux right)

equilibrada :: Arv a -> Bool
equilibrada F = True
equilibrada (N a left right) = equilibradaAux (alturas(N a left right))

equilibradaAux :: Arv Int -> Bool
equilibradaAux F = True
equilibradaAux (N a F F) = True
equilibradaAux (N a F (N b left right)) = b == 1
equilibradaAux (N a (N b left right) F) = b == 1
equilibradaAux (N a (N b left right) (N c left2 right2)) = b == c && equilibradaAux (N b left right) && equilibradaAux (N c left2 right2)