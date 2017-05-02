-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Examen de Febrero 2015.
--
-- Implementación del TAD Deque
--
-- Apellidos:
-- Nombre:
-- Grado en Ingeniería ...
-- Grupo:
-- Número de PC:
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueueClase
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity:
empty :: DEQue a
empty = DEQ [] []

-- Complexity:
isEmpty :: DEQue a -> Bool
isEmpty (DEQ xs ys) = null xs  && null ys

-- Complexity:
addFirst :: a -> DEQue a -> DEQue a
addFirst e (DEQ xs ys) = (DEQ (e:xs) ys)

-- Complexity:
addLast :: a -> DEQue a -> DEQue a
addLast e (DEQ xs ys) = (DEQ xs (e:ys))

-- Complexity:
first :: DEQue a -> a
first (DEQ xs ys) = if isEmpty (DEQ xs ys) 
   then error "La cola esta vacia" else if null xs
    then head (reverse ys) else head xs

-- Complexity:
last :: DEQue a -> a
last (DEQ xs ys) = if isEmpty (DEQ xs ys) 
   then error "La cola esta vacia" else if null ys
    then head (reverse xs) else head ys

-- Complexity:
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ xs ys) = if isEmpty (DEQ xs ys) 
   then error "La cola esta vacia" else if null xs
    then aux ys else (DEQ (drop 1 xs) ys)
      where
         aux ys = (DEQ (drop 1(reverse (drop (div (length ys) 2) ys))) (take (div (length ys) 2) ys))

-- Complexity:
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ xs ys) = if isEmpty (DEQ xs ys) 
   then error "La cola esta vacia" else if null ys
    then aux xs else (DEQ xs (drop 1 ys))
      where
         aux xs = if (mod (length xs) 2==0) 
            then (DEQ (take (div (length xs) 2) xs) (drop 1(reverse (drop (div (length xs) 2) xs))))
            else if length xs == 1 then DEQ [][] else (DEQ (take ((div (length xs) 2)+1) xs) (drop 1(reverse (drop ((div (length xs) 2)+1) xs))) )

instance (Show a) => Show (DEQue a) where
   show q = "DoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
