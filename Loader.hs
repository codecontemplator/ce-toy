{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Loader where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Types
import Optimizer

data Loader = Loader { 
    name :: String, 
    cost :: Int, 
    input :: KeySet, 
    output :: KeySet, 
    load :: Map Key Value -> IO (Map Key Value) 
  }

instance Show Loader where
  show x = name x

instance Edge Loader KeySet where
  source = input
  sink = output
  transition vs loader = vs `Set.union` (sink loader)
  weight = cost

instance Node KeySet where
  s1 .== s2 = s1 `Set.isSubsetOf` s2