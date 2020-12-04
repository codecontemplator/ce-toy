{-# LANGUAGE DeriveFunctor #-}

module Dsl where

import Types
import Free 

data RuleExprF a = 
      GetIntValue Key (Int -> a) 
    | GetAmount (Amount -> a)  
        deriving Functor

type RuleExpr = Free RuleExprF

getIntValue :: Key -> RuleExpr Int
getIntValue key = liftF $ GetIntValue key id

getAmount :: RuleExpr Amount
getAmount = liftF $ GetAmount id

data Rule = Rule String (RuleExpr Amount) | RAndThen Rule Rule

rule :: String -> RuleExpr Amount -> Rule
rule name expr = Rule name expr

andThen :: Rule -> Rule -> Rule
andThen r1 r2 = RAndThen r1 r2

