{-# LANGUAGE Rank2Types, TypeOperators, GeneralizedNewtypeDeriving #-}

module Eval where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.State.Lazy
import Types
import Loader
import Dsl
import Optimizer(solve)
import Free

--type f ~> g = forall x . f x -> g x

data RuleExprEvalState = RuleExprEvalState {
    currentAmount :: Amount,
    cache :: Map Key Value,
    loaders :: [Loader]
  } 

newtype RuleExprEvalT a = RuleExprEvalT { 
    runRuleExpr :: ExceptT String (StateT RuleExprEvalState IO) a 
  } deriving (Functor, Applicative, Monad, MonadState RuleExprEvalState, MonadError String, MonadIO)

-- https://repl.it/@daniel_brannstrom/lazy-vars#main.hs
getValue :: Key -> RuleExprEvalT Value
getValue key = do
    kvs <- cache <$> get
    case Map.lookup key kvs of
        Just value -> return value
        Nothing -> do
        ls <- loaders <$> get
        case ls of
            [] -> throwError $ "could not produce a value for key " ++ key
            (l:ls') -> do
            kvs' <- liftIO $ load l kvs          
            modify (\s -> s { cache = Map.union kvs kvs', loaders = ls' })
            getValue key

getIntValue' :: Key -> RuleExprEvalT Int
getIntValue' key = do
    value <- getValue key
    case value of 
        IntValue intValue -> return intValue
        _ -> throwError $ "invalid type"

getAmount' :: RuleExprEvalT Amount
getAmount' = currentAmount <$> get

ruleExprI :: RuleExprF ~> RuleExprEvalT
ruleExprI (GetIntValue key next) = do
    iv <- getIntValue' key 
    return $ next iv
ruleExprI (GetAmount next) = do
    amount <- getAmount'
    return $ next amount

-- assumption : get value commands are not conditional on neither amount not other variable values
getRuleExprKeys' :: RuleExpr a -> [String]
getRuleExprKeys' (Pure a) = []
getRuleExprKeys' (Free f) = 
    case f of
        (GetIntValue key next) -> key : getRuleExprKeys' (next 0)
        (GetAmount next)       -> getRuleExprKeys' (next 0)

getRuleKeys' :: Rule -> [String]
getRuleKeys' (Rule _ expr) = getRuleExprKeys' expr
getRuleKeys' (RAndThen r1 r2) = getRuleKeys' r1 ++ getRuleKeys' r2

ruleI :: Rule -> Amount -> Map Key Value -> [Loader] -> IO Amount
ruleI r = \amount initKeyValues loaders -> do
    let initKeys = Set.fromList $ Map.keys initKeyValues
    let requiredKeys = Set.fromList $ getRuleKeys' r
    let loaders' = solve initKeys requiredKeys loaders
    let initState = RuleExprEvalState {
        currentAmount = amount,
        cache = initKeyValues,
        loaders = loaders'
    }
    (amount,_) <- evalR' r initState
    return amount

-- https://repl.it/@daniel_brannstrom/CredEvalTest#eval.hs
evalR' :: Rule -> RuleExprEvalState -> IO (Amount,RuleExprEvalState)
evalR' (Rule name expr) state =  
    let 
        expr' = Free.interp ruleExprI expr
    in do
        (errorOrAmount,state') <- runStateT (runExceptT $ runRuleExpr expr') state 
        case errorOrAmount of
            Left str -> error $ "exception:" ++ str
            Right amount' -> return (amount',state')

evalR' (RAndThen r1 r2) state = do
  (amount,state') <- evalR' r1 state
  let state'' = state' {
      currentAmount = amount
  }
  evalR' r2 state''
