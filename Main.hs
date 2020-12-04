module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Process(ceProcessExample)
import Loader(Loader(..))
import Eval(ruleI)
import Types

loaders :: [Loader]
loaders = [
    Loader { 
      name = "zero", cost = 0, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Age" ],
      load = (\_ -> return $ Map.fromList [("Age",IntValue 90)])
    },
    Loader { 
      name = "small", cost = 1, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address" ],
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address", StringValue "Storgatan 20")])
    },
    Loader {
      name = "medium", cost = 2, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address", "Salary" ],
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address",StringValue "Storgatan 20"),("Salary",IntValue 300)])
    },
    Loader {
      name = "large", cost = 3, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address", "Salary", "NumReminders", "NumChildren", "CreditA", "CreditB" ],
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address",StringValue "Storgatan 20"),("Salary",IntValue 300),("NumReminders",IntValue 1),("NumChildren",IntValue 2),("CreditA",IntValue 10),("CreditB", IntValue 0)])
    }, 
    Loader {
      name = "scoring", cost = 0, 
      input = Set.fromList [ "Salary", "NumChildren", "NumReminders" ], 
      output = Set.fromList [ "Scoring" ],
      load = (\_ -> return $ Map.fromList [("Scoring",IntValue 3)])
    }
  ]

main :: IO ()
main = do
    let f = ruleI ceProcessExample
    let amount = 1800
    let initKeyValues = Map.fromList [("NationalId", StringValue "29840113")]  -- could reach out for local cache for more data
    result <- f amount initKeyValues loaders
    putStrLn $ "result=" ++ show result
