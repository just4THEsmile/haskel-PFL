module Stack (Stack, push, pop, top,  empty, isEmpty) where
data Stack a = Stk [a]

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


isOpen c = c `elem` "([{" 
isClose c = c `elem` ")]}"
matches '(' ')' = True
matches '[' ']' = True
matches '{' '}' = True
matches _ _ = False

parent :: String -> Bool
parent str = parentAux str empty

parentAux :: [Char] -> Stack -> Bool
parentAux [] stk = null stk 
parentAux (c:cs) stk
    | isOpen c = parentAux cs (c:stk)
    | isClose c =
        if null stk || not(matches(top stk) c)
            then False
            else parentAux cs (pop stk)
    | otherwise = parentAux cs stk
