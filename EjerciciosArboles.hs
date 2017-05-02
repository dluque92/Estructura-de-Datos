-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
--
-- Titulación: Grado en Ingeniería del Software
-- Alumno: LUQUE FERNÁNDEZ, DAVID
-- Fecha de entrega: 14 | 12 | 2015
--
-- Ejercicios de Árboles
--
-------------------------------------------------------------------------------
import Test.QuickCheck

data Tree a = Empty | Node a [Tree a] deriving Show

tamaño :: Tree a -> Int
tamaño Empty = 0
tamaño (Node x []) = 1
tamaño (Node x xs) = 1 + foldr (+) 0 [ tamaño x | x <- xs ]

listaHojas :: Tree a -> [a]
listaHojas Empty = []
listaHojas (Node x []) = [x]
listaHojas (Node x xs) = concat [listaHojas nd | nd <- xs]

valorMax :: (Ord a) => Tree a -> a
valorMax Empty = error "Árbol vacío"
valorMax (Node x []) = x
valorMax (Node x xs) = maximum (x : [valorMax ( Node p ps) | (Node p ps) <- xs])

preOrden :: Tree a -> [a]
preOrden Empty = []
preOrden (Node x []) = [x]
preOrden (Node x xs) = [x] ++ concat [ preOrden y | y <- xs]
