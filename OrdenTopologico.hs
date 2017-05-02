-----------------------------------
-- David Luque Fernández
-- Práctica 8
-- 2º Ing. Software - A
---------------------------------
module OrdenTopologico
(noPreds
, topSorting
) where

import DataStructures.Graph.DiGraph
import qualified DataStructures.Dictionary.AVLDictionary as D
import Data.List( (\\))

g1 :: DiGraph Int
g1 = mkDiGraphSuc [0,1,2,3,4,5,6] suc
  where
    suc 0 = [1,2,5]
    suc 1 = [4]
    suc 2 = []
    suc 3 = [2,4,5,6]
    suc 4 = []
    suc 5 = [2]
    suc 6 = [0,4]

noPreds :: (Eq a) => DiGraph a -> [a] -> [a]
noPreds g vs = [ v | v <- vs, null (preds v)]
  where
    preds v = [ w | w <- vs, elem v (successors g w )]

topSorting :: (Ord a) => DiGraph a -> [a]
topSorting g = aux (vertices g)
  where
    aux [] = []
    aux vs
      | null ws   = error "DiGraph is cyclic"
      | otherwise = v : aux (vs \\ [v])
      where
        ws = noPreds g vs
        v = head ws
--ordenTopologico :: Eq v => DiGraph v -> (Maybe [v], Maybe (Path v))
--ordenTopologico g
--  | null (DiGraph.vertices g') = (Just orderTop, Nothing)
--  | otherwise                   = (Nothing, Just $ extractCycle g')
--  where
--    (g', orderTop) = collectSources g []
--    collectSources :: Eq v => DiGraph v -> [v] -> (DiGraph v, [v])
--    collectSources g as
--      | null ss   = (g,as)
--      | otherwise = collectSources newg (as+ss)
--      where
--        ss = sources g
--        newg = deleteVertices ss g

--extractCycle :: Eq v => DiGraph v -> [v]
--extractCycle g = extractCycleAux (head $ DiGraph.vertices g) []
--  where
--    extractCycleAux v as
--      | v `elem` as = v : takeWhile (/=v) as ++ [v]
--      | otherwise   = extractCycleAux (head $ predecesors g v) (v:as)
