-- Part 1
import Stack
import State
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

createEmptyStack :: Stack
createEmptyStack = emptyStack

-- stack2Str :: Stack -> String
stack2Str = undefined -- TODO, Uncomment all the other function type declarations as you implement them

createEmptyState :: State
createEmptyState = emptyState 

-- state2Str :: State -> String
state2Str = undefined -- TODO

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

main :: IO()
main = do
    let stack = createEmptyStack :: Stack Int
    print stack