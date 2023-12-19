module Stack (Stack, push, pop, top,  empty, isEmpty) where


data Stack a = Stk [a] deriving Show

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False




openBrackets :: Char -> Bool
openBrackets char = char `elem` "([{"

closeBrackets :: Char -> Bool
closeBrackets char = char `elem` ")}]"

matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '[' ']' = True
matches '{' '}' = True
matches _ _ = False

processChar:: Stack Char -> Char -> Stack Char
processChar stack char
    | openBrackets char = push char stack
    | closeBrackets char =
        if not (isEmpty stack) && matches (top stack) char
            then pop stack
            else push char stack 
    | otherwise = stack


parent:: String -> Bool
parent = isEmpty . foldl processChar empty


calc:: Stack Float -> String -> Stack Float
calc(Stk(x:y:z)) "+" = Stk ((x+y):z)
calc(Stk(x:y:z)) "-" = Stk ((x-y):z)
calc(Stk(x:y:z)) "*" = Stk ((x*y):z)
calc(Stk(x:y:z)) "/" = Stk ((x/y):z)
calc stack numStr = push ( read numStr) stack

calcular:: String -> Float
calcular = top . foldl calc empty . words



