-------------------------------------------------------------------------------
-- Weighted Graph defined by list of vertices and adjancency function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.WeightedGraph
  ( WeightedGraph
  , WeightedEdge(WE)
  , Path
  , mkWeightedGraphEdges
  , mkWeightedGraphAdj
  , vertices
  , edges
  , weigthedEdges
  , successors
  , degree
  ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import Data.List(nub, intercalate, nubBy, delete, minimumBy, (\\))
import Data.Maybe(fromJust)

data WeightedEdge a w  = WE a w a deriving Show

type Path a  = [a] -- Path represented as list of vertices

data WeightedGraph a w  = WG [a] (a -> [(a,w)])

mkWeightedGraphAdj :: (Eq a) => [a] -> (a -> [(a,w)]) -> WeightedGraph a w
mkWeightedGraphAdj vs sucs  = WG (nub vs) sucs

mkWeightedGraphEdges :: (Eq a, Eq w) => [a] -> [WeightedEdge a w] -> WeightedGraph a w
mkWeightedGraphEdges vs es  = WG (nub vs) sucs
 where
   sucs v  = nub [ (y,w) | WE x w y <- es, x==v ]
                 ++
                 [ (x,w) | WE x w y <- es, y==v ]

successors :: WeightedGraph a w -> a -> [(a,w)]
successors (WG vs sucs) v  = sucs v

vertices :: WeightedGraph a w -> [a]
vertices (WG vs sucs)  = vs

edges :: (Eq a) => WeightedGraph a w -> [WeightedEdge a w]
edges (WG vs sucs)  = [ WE v w u | v <- vs, (u,w) <- sucs v ]

weigthedEdges :: WeightedGraph a w -> [WeightedEdge a w]
weigthedEdges (WG vs sucs)  = [ WE v w u | v <- vs, (u,w) <- sucs v ]

degree :: WeightedGraph a w -> a -> Int
degree g v  = length (successors g v)


instance (Eq a, Show a, Show w) => Show (WeightedGraph a w) where
  show g@(WG vs sucs)  = "WeightedGraph("++vertices++","++arcs++")"
   where
    vertices  = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map showEd $ nubBy cmp (edges g)) ++ ")"
    cmp (WE x _ y) (WE x' _ y')  = (x==x' && y==y') || (x==y' && y==x')
    showEd (WE x w y)  = intercalate "-" [ show x, show w, show y ]

-----------------------

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

-------------------------
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
