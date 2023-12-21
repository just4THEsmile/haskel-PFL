-- Part 1
import Stack
import State
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Expr = AddExpr Expr Expr
    | MultExpr Expr Expr
    | Num Int
    deriving (Eq, Show)

exec :: (Code, Stack DataType, State) -> (Code, Stack DataType, State)

--Empty
exec ([], stack, state) = ([], stack, state)

--Push n Instruction
exec (Push n:code, stack, state) = (code, push (Number n) stack, state)

-- Add Instruction
exec (Add:code, stack, state) = (code, push (Number (n1 + n2)) (pop (pop stack)), state)
  where
    n1 = case top stack of
      Number n -> n
      _ -> error "Run-time error"
    n2 = case top (pop stack) of
      Number n -> n
      _ -> error "Run-time error"

-- Mult Instruction
exec (Mult:code, stack, state) = (code, push (Number (n1 * n2)) (pop (pop stack)), state)
  where
    n1 = case top stack of
      Number n -> n
      _ -> error "Run-time error"
    n2 = case top (pop stack) of
      Number n -> n
      _ -> error "Run-time error"

-- Sub Instruction
exec (Sub:code, stack, state) = (code, push (Number (n1 - n2)) (pop (pop stack)), state)
  where
    n1 = case top stack of
      Number n -> n
      _ -> error "Run-time error"
    n2 = case top (pop stack) of
      Number n -> n
      _ -> error "Run-time error"

convertStr :: DataType -> String
convertStr TT = "True"
convertStr FF = "False"
convertStr (Number n) = show n

stack2Str :: Stack DataType -> String
stack2Str stk
  | isEmpty stk = ""
  | otherwise = convertStr (top stk) ++ remainingStack
  where
    remainingStack = if isEmpty (pop stk) then "" else "," ++ stack2Str (pop stk)
    
state2Str :: State -> String
state2Str [] = ""
state2Str [(var, value)] = var ++ "=" ++ convertStr value
state2Str ((var, value):xs) = var ++ "=" ++ convertStr value ++ "," ++ state2Str xs

run :: (Code, Stack DataType, State) -> (Code, Stack DataType, State)
run ([],stack,state) = ([],stack,state) 
run (code,stack,state) = run $ exec (code,stack,state)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO()
main = do
    let stack = push (Number 1) $ push TT $ push FF createEmptyStack
    print stack
    let result = stack2Str stack
    print result
    
    -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
    -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    -- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"
    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"
