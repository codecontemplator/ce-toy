{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
-- https://repl.it/@daniel_brannstrom/dataloaders#main.hs

module Optimizer where

import Data.Function (on)
import Data.List (minimumBy, partition, tails, inits)

class Node n => Edge e n | e -> n where
  source :: e -> n
  sink :: e -> n
  transition :: n -> e -> n
  weight :: e -> Int

class Node n where
  (.==) :: n -> n -> Bool

-- https://hackage.haskell.org/package/utility-ht-0.0.15/docs/src/Data.List.HT.Private.html#removeEach
removeEach :: [a] -> [(a, [a])]
removeEach =
     map (\(ys, pivot, zs) -> (pivot,ys++zs)) . splitEverywhere
  where
    splitEverywhere :: [a] -> [([a], a, [a])]
    splitEverywhere xs =
      map
          (\(y, zs0) ->
            case zs0 of
                z:zs -> (y,z,zs)
                [] -> error "splitEverywhere: empty list")
          (init (zip (inits xs) (tails xs)))

genSolve :: Edge e n => n -> n -> [e] -> [(Int,[e])]
genSolve curNode goalNode edges =
  let
    (directEdges, indirectEdges) = partition (\e -> source e .== curNode) [ e | e <- edges, not (sink e .== curNode) ]
  in
    if goalNode .== curNode then
      pure (0,[])
    else do
      (e,es) <- removeEach directEdges 
      (w,es') <- genSolve (transition curNode e) goalNode (es ++ indirectEdges)
      return (w + weight e, e:es')

-- todo: minimumBy assumes there is a solution (will result in a runtime exception foldl on empty list otherwise)
solve :: Edge e n => n -> n -> [e] -> [e]
solve curNode goalNode edges = snd $ minimumBy (compare `on` fst) (genSolve curNode goalNode edges)