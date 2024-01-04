maxpos :: [Int] -> Int
maxpos [] = 0
maxpos [a] = a
maxpos (x:xs) | x > (head xs) = maxpos ([x]++(tail xs))
              | otherwise = maxpos xs

dups :: [a]->[a]
dups xs = dupsAux xs True

dupsAux :: [a] -> Bool -> [a]
dupsAux [] _ = []
dupsAux (x:xs) True = x:x:(dupsAux xs False)
dupsAux (x:xs) False = x:(dupsAux xs True)

transforma:: String -> String
transforma [] = []
transforma (x:xs)
                | x `elem` ['a','e','i','o','u'] = x:'p':x:(transforma xs)
                | otherwise = x:(transforma xs)


type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta m = [head x | x <- m]:transposta[tail x | x <- m, tail x /=[]]

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (v:vs) (x:xs) = v*x + prodInterno vs xs

prodMat :: Matriz -> Matriz -> Matriz
prodMat [] [] = []
prodMat a b = [[prodInterno v1 v2| v2 <- (transposta b)]|v1 <-a]
            

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



f :: (a -> b -> c) -> b -> a -> c
f function b a = function a b