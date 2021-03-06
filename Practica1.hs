-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- CURSO 2015-16. Primera Práctica
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software
--
-- Alumno: LUQUE FERNÁNDEZ, DAVID
-- Fecha de entrega:  19 | 10 | 2015
--
--  Apartados resueltos: TODOS
--
-------------------------------------------------------------------------------

module Practica1 where
-- si quieres cambia el nombre del módulo con utilizando las iniciales de tus
-- apellidos y nombre

import Test.QuickCheck

-------------------------------------------------------------------------------
--
-- LEE DESPACIO TODOS LOS APARTADOS DE LOS DOS EJERCICIOS DE LA PRACTICA
--              =====
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- EJERCICIO 1

-- En este ejercicio escribiremos dos funciones para ordenar
-- ternas y comprobaremos que son la misma función a través de quickCheck
-- En el apartado (A) escribiremos una primera solución.
-- En el apartado (D) escribiremos una solución alternativa.
-- En el apartado (E) comprobaremos que computan los mismos valores.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 1.A Escribe el tipo de la función /ordena/, y completa las ecuaciones
-------------------------------------------------------------------------------
{-
*Practica1> ordena (1,2,-2)
(-2,1,2)
it :: (Integer, Integer, Integer)
-}

-- ordena :: ... -- completa la declaración siguiendo el esquema siguiente
ordena :: Ord a => (a,a,a) -> (a,a,a)
ordena (x,y,z)
    | x > y     =  ordena (y,x,z)
    | y > z     =  ordena (x,z,y)
    | otherwise = (x,y,z)
--  | ...

-------------------------------------------------------------------------------
-- 1.B Justifica que la función anterior 'ordena'  una terna
-------------------------------------------------------------------------------
-- La funcion compara la variable central con sus lados,
-- Si x > y entonces se intercambian y vuelve a llamar a ordena con sus valores intercambiados
-- Si y > z entonces se intercambian y vuelve a llamar a ordena con sus valores intercambiados
-- Si los valores estan en orden, devuelve la terna ordenada

-------------------------------------------------------------------------------
-- 1.C Define una función /inserta/ para insertar un elemento en un PAR ORDENADO
--     devolviendo una terna ordenada.
--     Da su tipo y sus ecuaciones.
--
-------------------------------------------------------------------------------
{-
*Practica1> inserta 3 (-1,2)
(-1,2,3)
it :: (Integer, Integer, Integer)
*Practica1> inserta 0 (-1,2)
(-1,0,2)
it :: (Integer, Integer, Integer)
-}

-- inserta :: ???  -- completa el tipo de /inserta/
--       COMPLETA las ecuaciones siguiendo el siguiente esquema
inserta :: Ord a => a -> (a,a) -> (a,a,a)
inserta x (p,q)
   | x <= p    = (x,p,q)
   | x <= q    = (p,x,q)
   | otherwise = (p,q,x)
-- | ...

-------------------------------------------------------------------------------
-- 1.D Es posible ordenar una terna (x,y,z) en dos pasos:
--               a) ordenamos el par (y,z)
--               b) insertamos x en el par obtenido en el paso anterior.
--     Define la función /ordena'/ que realiza el algoritmo anterior
-------------------------------------------------------------------------------

ordena' :: Ord a => (a,a,a) -> (a,a,a)
ordena' (x,y,z)
    | y <= z = inserta x (y,z)
    | y >  z = inserta x (z,y)

-------------------------------------------------------------------------------
-- 1.E Escribe una función para comprobar que /ordena/ y /ordena'/
--     son la misma función independientemente del tipo de la terna
-------------------------------------------------------------------------------

type T = Integer
-- type T = Char      -- prueba con las tres declaraciones
-- type T = Bool

check_ordena = quickCheck (p_ordena :: (T,T,T) -> Bool)

p_ordena t = ordena (t,t,t) == ordena' (t,t,t) -- sustituye el valor /undefined/

{-
*Practica1> check_ordena
+++ OK, passed 100 tests.
it :: ()
-}

-------------------------------------------------------------------------------
-- EJERCICIO 2.
--
-- Queremos escribir una función
--                cerosDe :: Integer -> Int
-- para calcular el número de ceros con que termina un entero arbitrario:
{- Ejemplos
*Practica1> cerosDe (-12003000)
3
it :: Int
*Practica1> cerosDe 120030000
4
-}
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 2.A   Escribe la función /cerosDe/ siguiendo el siguiente esquema
-------------------------------------------------------------------------------
cerosDe :: Integer -> Int
cerosDe x
   | x < 0     = cerosDe (-x)
   | x == 0    = 1
   | otherwise = if r == 0 then 1+cerosDe(x') else 0
-- | ... completa las ecuaciones con guardas ¡ HAY QUE CONTROLAR LA TERMINACIóN !

 where (x',r) = divMod x 10   -- divMod es de Prelude y calcula la división entera y el resto

-------------------------------------------------------------------------------
-- 2.B   Escribe una función /p_cerosDe/ para comprobar con quickCheck
-- el funcionamiento correcto vía el siguiente esquema:
--      1. Generamos un entero pequeño /n/, y un entero arbitrario /x/
--      2. Con ayuda de /x/ y de /n/ contruimos un número POSITIVO O NEGATIVO /m/
--         que termine en "exactamente" /n/ ceros.
--      3. Comprobamos que /cerosDe m/ devuelve /n/.
-------------------------------------------------------------------------------

-- p_cerosDe n x = ??  ==> ????
-- el antecedente /??/ debe servir para "controlar" que se genere un natural /n/ pequeño

p_cerosDe n x = 10>n && n>0 ==> cerosDe (x'*(10^n)) == n
   where
    x' = if mod x 10 == 0 then x+1 else x

{-
*Practica1> quickCheck p_cerosDe
*** Gave up! Passed only 20 tests.
*Practica1> quickCheck p_cerosDe
*** Gave up! Passed only 18 tests.
-}


-- EJERCICIO EXTRA
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
    | n < 0  = error "Introduce un digito mayor de 0"
    | n == 0 = (x:xs)
    | n > 0 = drop' (n-1) xs
