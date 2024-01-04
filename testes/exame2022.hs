type Species = (String, Int)
type Zoo = [Species]

isEndangered :: Species -> Bool
isEndangered (nome,numero) 
    | numero > 100 = False
    | otherwise = True

updateSpecies :: Species -> Int -> Species
updateSpecies (nome,antigo) novo = (nome,antigo+novo)

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (animal:animais) funcao
                                | funcao animal == True = [animal] ++ filterSpecies animais funcao 
                                | otherwise = filterSpecies animais funcao 

countAnimals :: Zoo -> Int
countAnimals animais = sum (map (\(nome,numero)->numero) animais)


substring :: (Integral a) => String -> a -> a -> String
substring str start end = [c | (i, c) <- zip [0..] str, i >= start, i <= end]

hasSubstr :: String -> String -> Bool
hasSubstr [] _ = False
hasSubstr _ [] = True
hasSubstr string substr =   if contains string substr
                            then True
                            else hasSubstr (tail string) substr

contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains string substr = (head(string)==head(substr)) && contains (tail string) (tail substr)


sortSpeciesWithSubstr :: Zoo -> String -> (Zoo,Zoo)
sortSpeciesWithSubstr zoo [] = (zoo,[])
sortSpeciesWithSubstr [] _ = ([],[])
sortSpeciesWithSubstr ((animal,numero):animais) substr =   
    if contains animal substr
    then let (zooWithSubstr, zooWithoutSubstr) = sortSpeciesWithSubstr animais substr
         in ((animal,numero):zooWithSubstr, zooWithoutSubstr)
    else let (zooWithSubstr, zooWithoutSubstr) = sortSpeciesWithSubstr animais substr
         in (zooWithSubstr, (animal,numero):zooWithoutSubstr)

rabbits :: [Integer]
rabbits = infiniteRabbits [2,3]

infiniteRabbits :: [Integer] -> [Integer]
infiniteRabbits lista = lista ++ infiniteRabbits (tail lista ++ [last lista + last (tail lista)])

infiniteRabbitsHelper :: (Integral a) => a -> [a] -> [a]
infiniteRabbitsHelper number lista = if number < (head(reverse lista))
                                     then lista
                                     else infiniteRabbitsHelper number (lista ++ [(( reverse lista )!! 0 )+( reverse lista )!! 1])

rabbitYears :: (Integral a) => a -> Int
rabbitYears 2 = 0
rabbitYears 3 = 1
rabbitYears number = length (infiniteRabbitsHelper number [2,3]) - 1


data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram
myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")


dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf _) = 0
dendroWidth (Node left dist right) = dist*2 + dendroWidthRoot left + dendroWidthRoot right

dendroWidthRoot :: Dendrogram -> Int
dendroWidthRoot (Leaf _) = 0
dendroWidthRoot (Node left dist right) = dist + dendroWidthRoot left + dendroWidthRoot right

dendroDistance :: Dendrogram -> Int -> Int -> String -> [String]
dendroDistance (Leaf s) dist bounded string = if dist >= bounded then [s] else []
dendroDistance (Node left dist right) maxdist bounded string = 
    if string == "left"
    then (dendroDistance left maxdist (bounded+dist) "left") ++  (dendroDistance right maxdist (bounded-dist) "left")
    else (dendroDistance left maxdist (bounded-dist) "right") ++  (dendroDistance right maxdist (bounded+dist) "right")

    
dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Leaf s) maxdist = [s]
dendroInBounds (Node left dist right) maxdist = (dendroDistance left maxdist dist "left") ++ (dendroDistance right maxdist dist "right")


