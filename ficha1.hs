import Data.List
--Teste Triangulo

testaTriangulo :: Int -> Int -> Int -> Bool
testaTriangulo a b c
    | (a + b > c) && (b + c > a) && (a + c > b) = True
    | otherwise = False


-- area Triangulo

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = 
    sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2


--Metades

metades :: [Int] -> ([Int],[Int])
metades a = (result1,result2)
    where
    len = length a
    half = len `div` 2
    result1 = take half a
    result2 = drop half a

-- Last


mylast :: [Int] -> Int
mylast list = head(reverse list)


-- Remove Last

myinit :: [Int] -> [Int]
myinit list = reverse(drop 1(reverse list))

--Binómio

binom :: Integer -> Integer -> Integer
binom n k = result
    where result = product[1..n]`div`(product[1..k]*product[1..(n-k)])

binom2 :: Integer -> Integer -> Integer
binom2 n k = result
    where result = product [n - k + 1 .. n] `div` product [1 .. k]




--Formula Resolvente

raizes :: Float -> Float -> Float -> (Float,Float)
raizes a b c = (x1,x2)
    where
      x1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
      x2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)


--Tipos e classes

{-

  a) [char]
  b) (char,char,char)
  c) [(Bool,char)]
  d) ([Bool],[Char])
  e) [([a] -> [a])]
  f) [(a -> a)]


  a) segundo :: [a] -> a
  b) trocar :: (a, b) -> (b, a)
  c) par :: a -> b -> (a, b)
  d) dobro :: Num a => a -> a
  e) metade :: Fractional -> Fractional
  f) minuscula :: Char -> Bool
  g) intervalo -> Ord a => a -> a -> a -> Bool
  h) palindromo :: Eq a => [a] -> Bool
  i) twice :: (a -> a) -> a -> a

-}

--Classifica

classifica :: Int -> String 
classifica n
  | n < 0 || n > 20  = "Erro"
  | n < 10 = "Reprovado"
  | n >= 10 && n < 13 = "Suficiente"
  | n >=13 && n < 16 = "Bom"
  | n >=16 && n < 19 = "Muito bom"
  | n >= 19 && n <21 = "Muito bom com distincao" 


-- Indice Massa Corporal

imc :: Float -> Float -> String
imc peso altura
  | resultado < 18.5 = "Baixo Peso"
  | resultado >= 18.5 && resultado < 25 = "Peso Normal"
  | resultado >= 25 && resultado <30 = "Excesso de peso"
  | resultado >= 30 = "Obesidade"
  where resultado = peso / (altura^2)

-- Max & Min

max3 :: Ord a => a -> a -> a -> a
max3 x y z = if x>=y && x >=z then x
             else if y >= x && y >= z then y
             else  z

min3 :: Ord a => a -> a -> a -> a
min3 x y z = if x<=y && x <= z then x 
             else if y <= x && y <= z then y
             else z


-- Função logica ou-exclusivo

xor :: Bool->Bool-> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = True

--SafeTail
safetail :: [a] -> [a]
safetail [] = []
safetail list = tail list

safetail2 :: [a] -> [a]
safetail2 list =  if length list == 0 then [] 
                  else tail list

safetail3 :: [a] -> [a]
safetail3 list 
    | length list == 0 = []
    | otherwise = tail list


--Lista Curta

curta :: [a] -> Bool
curta list = length list < 3 

curta2 :: [a] -> Bool
curta2 [] = True 
curta2 [_] = True
curta2 [_,_] = True
curta2 [_,_,_] = True
curta2 list = False


--Mediana

mediana :: Ord a => a -> a -> a -> a 
mediana a b c = sort [a, b, c] !! 1

mediana2 :: (Num a, Ord a) => a -> a -> a -> a
mediana2 a b c = (a+b+c) - min a (min b c) - max a (max b c)

--Converte
unidades :: [String]
unidades =
  [ "zero"
  , "um"
  , "dois"
  , "tres"
  , "quatro"
  , "cinco"
  , "seis"
  , "sete"
  , "oito"
  , "nove"
  ]

dez_a_dezanove :: [String]
dez_a_dezanove =
  [ "dez"
  , "onze"
  , "doze"
  , "treze"
  , "quatorze"
  , "quinze"
  , "dezasseis"
  , "dezassete"
  , "dezoito"
  , "dezanove"
  ]

dezenas :: [String]
dezenas =
  [ "vinte"
  , "trinta"
  , "quarenta"
  , "cinquenta"
  , "sessenta"
  , "setenta"
  , "oitenta"
  , "noventa"
  ]

centenas :: [String]
centenas =
  [ "cento"
  , "duzentos"
  , "trezentos"
  , "quatrocentos"
  , "quinhentos"
  , "seiscentos"
  , "setecentos"
  , "oitocentos"
  , "novecentos"
  ]



converte :: Int -> String
converte = converte6

converte6 :: Int -> String
converte6 n 
  | n<1000000 = combina6(divide6 n)

divide6 n = (n `div` 1000, n `mod` 1000)

combina6 (0, n) = converte3 n
combina6 (1, 0) = "mil"
combina6 (1, n) = "mil" ++ ligar n ++ converte3 n
combina6 (m, 0) = converte3 m ++ " mil"
combina6 (m, n) = converte3 m ++ " mil" ++ ligar n ++ converte3 n

ligar :: Int -> String
ligar r
  | r < 100 || r `mod` 100 == 0 = " e "
  | otherwise                   = " "

converte3 :: Int -> String
converte3 n | n<1000 = combina3 (divide3 n)

divide3 :: Int -> (Int, Int)
divide3 n = (n`div`100, n`mod`100)

combina3 :: (Int, Int) -> String
combina3 (0, n) = converte2 n
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c-1)
combina3 (c, n) = centenas !! (c-1) ++ " e " ++ converte2 n


converte2 :: Int -> String
converte2 n | n<100 = combina2 (divide2 n)

divide2 :: Int -> (Int, Int)
divide2 n = (n`div`10, n`mod`10)

combina2 :: (Int, Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u