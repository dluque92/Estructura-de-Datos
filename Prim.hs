-----------------------------------
-- David Luque Fernández
-- Práctica 8
-- 2º Ing. Software - A
---------------------------------
module Prim
(
, prim
) where

import DataStructures.Graph.WeightedGraph
import qualified DataStructures.Dictionary.AVLDictionary as D
import Data.List(nub, intercalate, nubBy, delete, minimumBy, (\\))
import Data.Maybe(fromJust)

-- Prim
g3 :: WeightedGraph Char Int
g3 = mkWeightedGraphEdges ['a','b','c','d','e','f','g']
                              [ WE 'a' 7 'b', WE 'a' 5 'd'
                              , WE 'b' 9 'd', WE 'b' 8 'c', WE 'b' 7 'e'
                              , WE 'c' 5 'e'
                              , WE 'd' 15 'e', WE 'd' 6 'f'
                              , WE 'e' 8 'f', WE 'e' 9 'g'
                              , WE 'f' 11 'g' ]


prim :: (Eq a, Ord w) => WeightedGraph a w -> [WeightedEdge a w]
prim g = aux [v] vs []
 where
    (v:vs) = vertices g
    aux _ [] st = st
    aux t r st = aux (y:t) (r\\[y]) (WE x p y : st)
        where
            WE x p y = minimumBy comparaArcos [WE v w u | v <- t, (u,w) <- successors g v, elem u r]
            comparaArcos (WE _ p1 _) (WE _ p2 _)
              | p1 == p2 = EQ
              | p1 < p2 = LT
              | otherwise = GT
