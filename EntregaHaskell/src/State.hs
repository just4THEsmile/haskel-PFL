module State (State,DataType(..) ,createEmptyState) where

data DataType= Number Integer | TT | FF 
    deriving (Eq, Show)

type State = [(String, DataType)]

createEmptyState :: State
createEmptyState = []
