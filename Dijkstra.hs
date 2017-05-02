-----------------------------------
-- David Luque Fernández
-- Práctica 8
-- 2º Ing. Software - A
---------------------------------
module Dijkstra
(shortestPaths
) where

import DataStructures.Graph.WeightedGraph
import qualified DataStructures.Dictionary.AVLDictionary as D
import Data.List(nub, intercalate, nubBy, delete, minimumBy, (\\))
import Data.Maybe(fromJust)

-- Dijkstra
g1 :: WeightedGraph Char Int
g1 = mkWeightedGraphEdges [ 'a','b','c','d','e']
                           [ WE 'a' 3 'b', WE 'a' 7 'd'
                           , WE 'b' 4 'c', WE 'b' 2 'd'
                           , WE 'c' 5 'd', WE 'c' 6 'e'
                           , WE 'd' 5 'e' ]


shortestPaths :: (Ord a, Ord w, Num w) => WeightedGraph a w -> a -> [Path a]
shortestPaths g v0 = aux v [v0] cpopt
  where
    v = delete v0 (vertices g)
    cpopt = D.insert v0 (0,[v0]) D.empty
    aux v vopt cpopt
      | v == [] = [ snd x | x <- D.values cpopt ]
      | otherwise = aux (delete h v) ([h] ++ vopt) (D.insert h (z, snd(fromJust (D.valueOf x cpopt))++[h]) cpopt)
      where
        caminos = [ (y,w,x) | x <- vopt, (y,w) <- successors g x, elem y v ]
        (z,h,x) = minimum [ (w + fst (fromJust (D.valueOf x cpopt)),y, x) | (y,w,x) <- caminos ]
