module LowLevelMachine where
-- Part 1
import Stack
import State
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Execute Instructions
exec :: (Code, Stack DataType, State) -> (Code, Stack DataType, State)

--Empty
exec ([], stack, state) = ([], stack, state)

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

--Equal Instruction
exec (Equ:code, stack, state) = 
  case (top stack, top(pop stack)) of
    (Number n1, Number n2) -> (code, push (if n1 == n2 then TT else FF) (pop (pop stack)), state)
    (TT, TT) -> (code, push TT (pop (pop stack)), state)
    (FF, FF) -> (code, push TT (pop (pop stack)), state)
    (_,_)-> error "Run-time error"

--Lower or Equal Instruction
exec(Le:code, stack, state) =
  case (top stack, top(pop stack)) of
    (Number n1, Number n2) -> (code, push (if n1 <= n2 then TT else FF) (pop (pop stack)), state)
    (_,_)-> error "Run-time error"

--Push n Instruction
exec (Push n:code, stack, state) = (code, push (Number n) stack, state)

--Fetch x Instruction
exec(Fetch x:code, stack, state) = (code, push (stateLookup state x) stack, state)
  where
    stateLookup [] v = error ("Variable " ++ v ++ " not found") 
    stateLookup ((var, value):xs) v
      | var == v = value
      | otherwise = stateLookup xs v

--Store x Instruction
exec (Store x:code, stack, state) = (code, pop stack, storeVal state (x, top stack))
 where
    storeVal [] (x, value) = [(x, value)]
    storeVal ((var, value):xs) (x, value2)
      | var == x = (x, value2):xs
      | otherwise = (var, value):storeVal xs (x, value2)

--Negation Instruction
exec(Neg:code, stack, state) = 
  case top stack of
      TT -> (code, push FF (pop stack), state)
      FF -> (code, push TT (pop stack), state)
      _  -> error "Run-time error"

--Tru Instruction
exec (Tru:code, stack, state) = (code, push TT stack, state)

--Fals Instruction
exec (Fals:code, stack, state) = (code, push FF stack, state)

--And Instruction  //////Dont forget error
exec (And:code, stack, state) 
  | isEmpty stack || isEmpty (pop stack) = error "Run-time error"
  | otherwise = 
      case (top stack, top (pop stack)) of
        (TT, TT) -> (code, push TT (pop (pop stack)), state)
        (FF, FF) -> (code, push FF (pop (pop stack)), state)
        (TT, FF) -> (code, push FF (pop (pop stack)), state)
        (FF, TT) -> (code, push FF (pop (pop stack)), state)
        (_,_) -> error "Run-time error"

--Branch Instruction
exec (Branch code1 code2:code, stack, state) = 
  case top stack of
    TT -> (code1 ++ code, pop stack, state)
    FF -> (code2 ++ code, pop stack, state)
    _ -> error "Run-time error"

--Loop Instruction
exec (Loop code1 code2:code, stack, state) = 
  case top stack of
    TT -> (code1 ++ [Loop code1 code2] ++ code, pop stack, state)
    FF -> (code2 ++ code, pop stack, state)
    _ -> error "Run-time error"

--Noop Instruction
exec (Noop:code, stack, state) = (code, stack, state)


-- Convert From DataType to String
convertStr :: DataType -> String
convertStr TT = "True"
convertStr FF = "False"
convertStr (Number n) = show n

-- Convert From Stack to String
stack2Str :: Stack DataType -> String
stack2Str stk
  | isEmpty stk = ""
  | otherwise = convertStr (top stk) ++ remainingStack
  where
    remainingStack = if isEmpty (pop stk) then "" else "," ++ stack2Str (pop stk)

-- Sort State, According to testAssembler cases
sortState :: State -> State
sortState [] = []
sortState (x:xs) = insert x (sortState xs)
  where
    insert x [] = [x]
    insert (var,value) ((var2,value2):xs)
      | var < var2 = (var,value):(var2,value2):xs
      | otherwise = (var2,value2):insert (var,value) xs

-- Convert From State to String
state2Str :: State -> String
state2Str state = state2StrSorted (sortState state)
  where
    state2StrSorted [] = ""
    state2StrSorted [(var, value)] = var ++ "=" ++ convertStr value
    state2StrSorted ((var, value):xs) = var ++ "=" ++ convertStr value ++ "," ++ state2Str xs


-- Run Machine
run :: (Code, Stack DataType, State) -> (Code, Stack DataType, State)
run ([],stack,state) = ([],stack,state) 
run (code,stack,state) = run $ exec (code,stack,state)

-- Test Assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

mainAssembler :: IO()
mainAssembler = do
    --testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult]

    -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    --print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"]

    -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    --print $ testAssembler [Fals,Store "var",Fetch "var"]

    -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    -- print $ testAssembler [Push (-20),Tru,Fals]

    -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    -- print $ testAssembler [Push (-20),Tru,Tru,Neg]

    -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    -- print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ]

    -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
    -- print $ testAssembler [Push (-20),Push (-21), Le]

    -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"]

    -- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    -- print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]

    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- print $ testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"

    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- print $ testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"
