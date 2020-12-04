module Types where

import Data.Set (Set)

type Key = String
data Value = IntValue Int | StringValue String deriving (Show,Eq)
type Amount = Int
type Age = Int
type KeySet = Set Key
type Decision = (String,Amount,Amount)