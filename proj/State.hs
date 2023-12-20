module State (State, emptyState) where

data State = State [(String, Integer)]

emptyState :: State
emptyState = State []

