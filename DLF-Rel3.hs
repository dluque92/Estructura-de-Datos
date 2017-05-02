-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: LUQUE FERNÁNDEZ, DAVID
-- Fecha de entrega: 16 | 11 | 2015
--
-- Relación de Ejercicios 3. Ejercicios resueltos: 11
--
-------------------------------------------------------------------------------
module Bag
  ( Bag
  , empty
  , isEmpty
  , insert
  , delete
  , occurrences
  , union
  , interseccion
  , diferencia
  , enSaco
  ) where
import Test.QuickCheck
import Data.List(intercalate)

-- EJERCICIO 11

data Bag a = Empty | Node a Int (Bag a) deriving Eq

-- Devuelve un saco bacío
empty :: Bag a
empty = Empty

-- Comprueba si un saco está vacía
isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Inserta una nueva ocurrencia
insert :: Ord a => a -> Bag a -> Bag a
insert x Empty = Node x 1 (Empty)
insert x (Node y ys s)
    | x < y   = Node x 1 (Node y ys s)
    | x == y  = Node y (ys+1) s
    | x > y   = Node y ys (insert x s)

-- Devuelve el número de ocurrencias de un elemento en un saco
-- (0 si el elemento no está)
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty = 0
occurrences x (Node y ys s)
    | x < y   = 0
    | x == y  = ys
    | x > y   = occurrences x s

-- Borra una ocurrencia de un dato de un saco
-- Devuelve el mismo saco si el elemento no estaba incluido
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty = Empty
delete x (Node y ys s)
    | x < y               = Node y ys s
    | x == y && (ys > 1)  = Node y (ys-1) s
    | x == y && (ys == 1) = s
    | x > y               = Node y ys (delete x s)

instance (Show a) => Show (Bag a) where
  show s = "Bag [ " ++ intercalate ", " (aux s) ++ "]"
    where
      aux Empty = []
      aux (Node x y z) = show (x, y) : aux z

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary =  do
                  xs <- listOf arbitrary
                  return (foldr insert empty xs)

b1 :: Bag Int
b1 = insert 2 empty

b2 :: Bag Int
b2 = insert 1 (insert 2 empty)

p_isEmpty_empty = isEmpty empty == True
p_isEmpty_insert x s = isEmpty (insert x s) == False

p_occurrences_empty x = occurrences x empty == 0
p_occurrences_insert_1 x s = occurrences x (insert x s) == (occurrences x s) + 1
p_occurrences_insert_2 x y s = x /= y ==> occurrences x (insert y s) == occurrences x s

p_delete_empty x = delete x empty == Empty
p_delete_insert_1 x s = delete x (insert x s) == s
p_delete_insert_2 x y s  = x /= y ==> delete x (insert y s) == insert y (delete x s)

union :: Ord a => Bag a -> Bag a -> Bag a
union s Empty = s
union Empty s =  s
union (Node x i b) (Node y j v)
  | x == y    = Node x (i+j) (union b v)
  | x <  y    = Node x i (union b (Node y j v))
  | otherwise = Node y j (union v (Node x i b))

interseccion :: Ord a => Bag a -> Bag a -> Bag a
interseccion s Empty = Empty
interseccion Empty s = Empty
interseccion (Node x ox s) (Node y oy t)
    | x > y              = interseccion t (Node x ox s)
    | x == y && ox <= oy = Node x ox (interseccion s t)
    | x == y && ox > oy  = Node x oy (interseccion s t)
    | x < y              = interseccion s (Node y oy t)

diferencia :: Ord a => Bag a -> Bag a -> Bag a
diferencia s Empty = s
diferencia Empty s = Empty
diferencia (Node x ox s) (Node y oy t) = diferencia (delete y (Node x ox s)) (delete y (Node y oy t))

enSaco :: Ord a => Bag a -> Bag a -> Bool
enSaco Empty t = True
enSaco t Empty = False
enSaco (Node x f s) (Node y g t)
  | x == y && f <= g = enSaco s t
  | x == y && f > g = False
  | x <  y = False
  | x >  y = enSaco (Node x f s) t

-- propiedades QuickCheck para comprobar union, intersection y diferencia
p_union xs ys = union xs ys == union ys xs

p_interseccion xs ys = interseccion xs ys == interseccion ys xs

p_diferencia s = diferencia s s == Empty
