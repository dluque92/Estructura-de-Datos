-----------------------------------
-- David Luque Fernández
-- Práctica 8
-- 2º Ing. Software - A
---------------------------------
module Euler
(cicloPequeño
, hayCamino
, borrarArista
, borrarVertice
--, caminoEuler
, nuevoGrafo
) where

import DataStructures.Graph.Graph
import Data.String

g1 :: Graph Int
g1 = mkGraphEdges [1,2,3,4,5,6,7,8,9] [(1,2), (1,3), (1,7), (1,8), (2,3), (3,4), (3,7), (4,5), (4,7), (5,9), (6,7), (6,9), (7,8), (7,9) ]


g2 :: Graph Int
g2 = mkGraphSuc [1,2,3,4,5,6,7,8,9,10] sucs
  where
    sucs 1 = [2,3,7,8]
    sucs 2 = [1,3]
    sucs 3 = [1,2,4,7]
    sucs 4 = [3,5,7,9]
    sucs 5 = [4,9]
    sucs 6 = [7,9]
    sucs 7 = [1,3,4,6,8,9]
    sucs 8 = [1,7]
    sucs 9 = [4,5,6,7]
    sucs 10 = []

hayCamino :: Graph a -> Bool
hayCamino g = foldr (==) True [ even (length (successors g x)) | x <- (vertices g) ]

borrarArista :: (Eq a) => Graph a -> a -> a -> Graph a
borrarArista g v1 v2 = mkGraphEdges (vertices g) sucesores
  where
    sucesores = [ (x,y) | x <- (vertices g) , y <- (successors g x), (x,y) /= (v1,v2), (x,y) /= (v2,v1) ]

borrarVertice :: (Eq a) => Graph a -> Graph a
borrarVertice g = mkGraphEdges vert sucesores
  where
    vert = [x | x <- (vertices g), (null (successors g x)) == False ]
    sucesores = [ (x,y) | x <- (vertices g) , y <- (successors g x) ]

cicloPequeño :: (Eq a) => Graph a -> a -> [(a,a)]
cicloPequeño g v = aux g v
  where
    aux grafo origen
      | v == destino = [(origen,destino)]
      | otherwise = (origen,destino) : aux grafo'' destino
        where
          destino = if degree grafo (head (successors grafo origen)) > 1 then (head (successors grafo origen)) else (last (successors grafo origen))
          grafo' = borrarArista grafo origen destino
          grafo'' = borrarVertice grafo'
          --destino' = head [x | x <- (successors grafo'' destino) , (degree grafo'' x) > 0]

nuevoGrafo :: (Eq a) => Graph a -> [(a,a)] -> Graph a
nuevoGrafo g aristas = if null aristas then g else nuevoGrafo g'' camino'
    where
      (v1,v2) = head aristas
      camino' = drop 1 aristas
      g'  = borrarArista g v1 v2
      g'' = borrarVertice g'

caminoEuler :: (Eq a) => Graph a -> a -> [a]
caminoEuler g origen = if length (vertices(grafo2))/=0 then nuevoCamino ++ (caminoEuler grafo2 origen) else nuevoCamino ++ [origen]
  where
    nuevoCamino = map fst camino
    camino = cicloPequeño g origen
    grafo2 = nuevoGrafo g camino
    --aux g origen' = if origen == origen' then nuevoCamino ++ (caminoEuler grafo2 origen) else [origen'] ++ (caminoEuler grafo2 (head (drop 1 nuevoCamino))) ++ [origen']
    --origen' = if (degree g origen) >0 then origen else (head (drop 1 nuevoCamino))

    --nuevoCamino ++ (caminoEuler grafo2 origen)
    --then if(degree g2 (head nuevoCamino) >0)
    --aux =       else origen ++ (caminoEuler grafo2 (head (drop 1 nuevoCamino))) ++ (drop 1 nuevoCamino)
    --aux g origen = if (degree g origen > 0)  then aux g origen else caminoEuler g (head[x | x <-(successors g origen), degree g x >1])
