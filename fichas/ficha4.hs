data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No n left right) = n + sumArv left + sumArv right

listar :: Arv a -> [a]
listar Vazia = []
listar (No n left right) = listar right ++ [n] ++ listar left

nivel:: Int ->Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No n left right) = [n]  
nivel int (No n left right) = (nivel (int-1) left) ++ (nivel (int-1) right)

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No x left right) = No (f x) (mapArv f left) (mapArv f right)


mais_dir :: Arv a -> a
mais_dir Vazia = error "Empty Tree"
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ right) = mais_dir right


remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia
remover x (No y left right) 
    | x < y = No y (remover x left) right
    | x > y = No y left (remover x right)
    | otherwise = removerNo (No y left right)

removerNo :: Ord a=> Arv a  -> Arv a
removerNo Vazia = error "Empty Tree"
removerNo (No _ Vazia right) = right
removerNo (No _  left Vazia) = left
removerNo (No _  left right) = No n (remover n left) right
    where n = mais_dir left


invertInput :: IO ()
invertInput = do 
    str <- getLine
    putStrLn $ reverse str

main :: IO ()
main = do
    let tree = No 1 (No 2 Vazia Vazia) (No 3 Vazia Vazia)

    -- Test sumArv
    print $ sumArv tree -- Should print 6

    -- Test listar
    print $ listar tree -- Should print [3, 1, 2]

    -- Test nivel
    print $ nivel 0 tree -- Should print [1]
    print $ nivel 1 tree -- Should print [2, 3]

    -- Test mapArv
    let newTree = mapArv (+1) tree
    print $ listar newTree -- Should print [4, 2, 3]

    --Test mais_dir
    print $ mais_dir tree --Should print 3

    -- Test remover
    let newTree = remover 2 tree
    print $ listar newTree -- Should print [3, 1]