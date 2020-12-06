{-# LANGUAGE DeriveFunctor #-}

module Dsl where

import Types
import Free 

data RuleExprF a = 
      GetIntValue Key (Int -> a) 
    | GetStringValue Key (String -> a)
    | GetAmount (Amount -> a)  
    | IfB Bool (RuleExpr a) (RuleExpr a)
        deriving Functor

type RuleExpr = Free RuleExprF

getIntValue :: Key -> RuleExpr Int
getIntValue key = liftF $ GetIntValue key id

getStringValue :: Key -> RuleExpr String
getStringValue key = liftF $ GetStringValue key id

getAmount :: RuleExpr Amount
getAmount = liftF $ GetAmount id

ifB :: Bool -> RuleExpr a -> RuleExpr a -> RuleExpr a
ifB b t f = liftF $ IfB b t f

data Rule = Rule String (RuleExpr Amount) | RAndThen Rule Rule

rule :: String -> RuleExpr Amount -> Rule
rule name expr = Rule name expr

andThen :: Rule -> Rule -> Rule
andThen r1 r2 = RAndThen r1 r2

class IfThenElse b where
  ifThenElse :: b -> RuleExpr a -> RuleExpr a -> RuleExpr a

instance IfThenElse Bool where
  ifThenElse b t f = ifB b t f

accept :: RuleExpr Amount
accept = getAmount

reject :: RuleExpr Amount
reject = return 0

acceptLimit :: Amount -> RuleExpr Amount
acceptLimit limit = return limit
