isLeapYear :: Integer -> Bool
isLeapYear year = if year `mod` 4 == 0
                  then 
                    if year `mod` 100 == 0
                    then 
                        if year `mod` 400 == 0
                        then True
                        else False
                    else True
                  else False


data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds| planet == Mercury = earthAge/0.2408467
                    | planet == Venus = earthAge/0.61519726
                    | planet == Earth = earthAge
                    | planet == Mars = earthAge/1.8808158
                    | planet == Jupiter = earthAge/11.862615
                    | planet == Saturn = earthAge/29.447498
                    | planet == Uranus = earthAge/84.016846
                    | planet == Neptune = earthAge/164.79132
                    where earthAge = ((((seconds / 60) / 60)/24)/365.25)
                    

isPangram :: String -> Bool
isPangram str = all (`elem` lowerStr) ['a'..'z']
    where 
        lowerStr = [toLower c | c <- str]
        toLower c| 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
                 | otherwise = c


collatzAux :: Integer -> Integer -> Maybe Integer
collatzAux steps 1 = Just steps
collatzAux steps n = if n `mod` 2 == 0
                     then collatzAux (steps+1) (n `div` 2)
                     else collatzAux (steps+1) (n*3+1)

collatz :: Integer -> Maybe Integer
collatz 0 = Nothing
collatz n = if n > 0
            then collatzAux 0 n 
            else Nothing


tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a,b,c)|a<-[1..sum],b<-[a..sum],let c = sum - a - b, a^2+b^2==c^2]

toRNA :: String -> Either Char String
toRNA [] = Right ""
toRNA (x:xs)
  | x == 'G' = fmap ('C' :) (toRNA xs)
  | x == 'C' = fmap ('G' :) (toRNA xs)
  | x == 'T' = fmap ('A' :) (toRNA xs)
  | x == 'A' = fmap ('U' :) (toRNA xs)
  | otherwise = Left x


sumOfMultiplesAux :: [Integer] -> Integer -> [Integer]
sumOfMultiplesAux [] _ = []
sumOfMultiplesAux (factor:factors) limit = if factor > 0
                                           then [ n| n<-[1..(limit-1)], n `mod` factor == 0 ] ++ (sumOfMultiplesAux factors limit)
                                           else (sumOfMultiplesAux factors limit)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum(removeDups(sumOfMultiplesAux factors limit) [])

removeDups :: [Integer] -> [Integer] -> [Integer]
removeDups [] lista = reverse lista
removeDups (x:xs) lista
  | x `notElem` lista = removeDups xs (x:lista)
  | otherwise = removeDups xs lista

