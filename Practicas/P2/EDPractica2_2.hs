-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- PRACTICA 2ª (Características de la Programación Funcional)
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería ................ [Informática | del Software | de Computadores].
-- Alumno: APELLIDOS, NOMBRE
-- Fecha de entrega:  DIA | MES | AÑO
--
-- Ejercicios resueltos de la Relación : ..........
--
-------------------------------------------------------------------------------
module Practica2 where

import Test.QuickCheck


-------------------------------------------------------------------------------
-- Ejercicio 3
-------------------------------------------------------------------------------

reparte :: [a] -> ([a],[a])
-- Reparte los elementos de la lista original en dos listas de alternativamente
-- cada elemento va cayendo en una de las dos listas solución.
reparte [] = ([],[])
reparte [x] = ([x],[])
reparte (x:y:xs) = (x:us,y:vs)
	where 
		(us, vs) = reparte xs

-------------------------------------------------------------------------------
-- Ejercicio 6
-------------------------------------------------------------------------------

divisores :: Integer -> [Integer]
-- divisores naturales (>=0) de un entero
divisores x = [ y | y <- [1..x] , mod x y == 0]

divisores' :: Integer -> [Integer]
-- divisores enteros (positivos y negativos) de un entero
divisores' x = if x<0 then divisores' (-x) else [ y | y <- [(-x)..(-1)]++[1..x] , mod (x) (y) == 0, y /= 0]

-------------------------------------------------------------------------------
-- Ejercicio 8
-------------------------------------------------------------------------------
-- ¿es primo un número ?

esPrimo :: Integer -> Bool
esPrimo  x = if head [ y | y <- [2..x], mod x y == 0] == x then True else False

primosHasta  m = [ x | x <- [2..m], esPrimo x]

-------------------------------------------------------------------------------
-- Ejercicio 10 . Número perfecto
-------------------------------------------------------------------------------
-- Un número n es perfecto si la suma de sus divisores (sin incluir al propio n) es n

-- Indica si un número dado es perfecto
esPerfecto :: Integer -> Bool
esPerfecto n = if sum [x | x <- [1..(n-1)], mod n x == 0] == n then True else False

-- Calcula todos los números perfectos hasta un tope dado
-- perfectosHasta ::
perfectoHasta n = [ x | x <- [1..n], esPerfecto x ]

-------------------------------------------------------------------------------
-- Ejercicio 37
-------------------------------------------------------------------------------

type Izdo = Double
type Dcho = Double
type Epsilon = Double
type Función = Double -> Double
bipartición :: Función -> Izdo -> Dcho -> Epsilon -> Double

bipartición f a b epsilon
  | long < epsilon    = undefined
-- sigue aqui
  where
      long = b - a
-- sigue aqui
