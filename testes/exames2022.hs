type Species = (String, Int)
type Zoo = [Species]

--[("zebra",100),("macaco",99),("humano",1000)]

isEndangered :: Species -> Bool
isEndangered (especie,numero)
    | numero > 100 = False
    | otherwise = True

updateSpecies :: Species -> Int -> Species
updateSpecies (especie,numero) bebes = (especie,numero+bebes)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] funcao = []
filterSpecies (animal:animais) funcao
    | funcao animal = animal:(filterSpecies animais funcao)
    | otherwise = filterSpecies animais funcao

countAnimals :: Zoo -> Int
countAnimals zoo = sum (map snd zoo)

substring :: (Integral a) => String -> a -> a -> String
substring str inicial final = [x|(i,x)<-zip [0..] str, i >= inicial, i<final]


hasSubstrAux :: String -> String -> Bool
hasSubstrAux [] [] = True
hasSubstrAux [] _ = False
hasSubstrAux _ [] = True
hasSubstrAux string substr = (head(string) ==head(substr)) && hasSubstrAux (tail string) (tail substr)

hasSubstr :: String -> String -> Bool
hasSubstr [] _ = False
hasSubstr string substr =
    if hasSubstrAux string substr
    then True
    else hasSubstr (tail string) substr
    

sortSpeciesWithSubstrAux :: Zoo -> String -> (Zoo, Zoo) -> (Zoo,Zoo)
sortSpeciesWithSubstrAux [] _ final = final
sortSpeciesWithSubstrAux (animal:animais) string (sim,nao)
    | hasSubstr (fst animal) string = sortSpeciesWithSubstrAux animais string (animal:sim, nao)
    | otherwise = sortSpeciesWithSubstrAux animais string (sim, animal:nao)

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo string = sortSpeciesWithSubstrAux zoo string ([],[])


rabbits :: (Integral a) => [a]
rabbits = rabbitsAux 2 3
  where
    rabbitsAux a b = a : rabbitsAux b (a + b)


rabbitYears :: (Integral a) => a -> Int
rabbitYears n = rabbitAux 2 3 !! fromIntegral n
  where
    rabbitAux a b = a : rabbitAux b (a + b)



data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram

myDendro :: Dendrogram
-- myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Node (Leaf "cactus") 4 (Leaf "monkey"))
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

dendroWidthNotRoot :: Dendrogram -> Int
dendroWidthNotRoot (Leaf _) = 0
dendroWidthNotRoot (Node left dist right) = dist + (dendroWidthNotRoot left) + (dendroWidthNotRoot right)

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf _) = 0
dendroWidth (Node left dist right) = dist*2 + (dendroWidthNotRoot left) + (dendroWidthNotRoot right)


dendroDistance :: Dendrogram -> Int -> Int -> String -> [String]
dendroDistance (Leaf s) dist bounded string = if dist >= bounded then [s] else []
dendroDistance (Node left dist right) maxdist bounded string = 
    if string == "left"
    then (dendroDistance left maxdist (bounded+dist) "left") ++  (dendroDistance right maxdist (bounded-dist) "left")
    else (dendroDistance left maxdist (bounded-dist) "right") ++  (dendroDistance right maxdist (bounded+dist) "right")

    
dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Leaf s) maxdist = [s]
dendroInBounds (Node left dist right) maxdist = (dendroDistance left maxdist dist "left") ++ (dendroDistance right maxdist dist "right")