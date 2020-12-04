module Main where

import Process(ceProcessExample)
import Loader(Loader(..))
import Eval(ruleI)

loaders :: [Loader]
loaders = [
    Loader { 
      name = "small", cost = 1, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address" ],
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address", StringValue "Storgatan 20")])
    },
    Loader {
      name = "medium", cost = 2, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address", "Salary" ] 
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address",StringValue "Storgatan 20"),("Salary",IntValue 300)])
    },
    Loader {
      name = "large", cost = 3, 
      input = Set.fromList [ "NationalId" ], 
      output = Set.fromList [ "Name", "Address", "Salary", "NumReminders", "NumChildren" ] 
      load = (\_ -> return $ Map.fromList [("Name",StringValue "Pelle"),("Address",StringValue "Storgatan 20"),("Salary",IntValue 300),("NumReminders",IntValue 1),("NumChildren",IntValue 2)])
    }, 
    Loader {
      name = "scoring", cost = 0, 
      input = Set.fromList [ "Salary", "NumChildren", "NumReminders" ], 
      output = Set.fromList [ "Scoring" ] 
      load = (\_ -> return $ Map.fromList [("Scoring",IntValue 3)])
    }
  ]

main :: IO ()
main = do
    putStrLn "Start!"
    let f = evalR ceProcessExample
    let amount = 800
    let initKeyValues = Map.FromList [("NationalId", StringValue "29840113")]  -- could reach out for local cache for more data
    amount' <- f amount initKeyValues loaders
    putStrLn $ "amount=" ++ show amount'
    putStrLn "Done!"
