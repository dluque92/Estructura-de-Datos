-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
--
-- Titulación: Grado en Ingeniería del Software
-- Alumno: LUQUE FERNÁNDEZ, DAVID
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck

-- Ejercicio 1
-- A
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = (x*x)+(y*y) == (z*z)

-- B
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = if x > y then ((x*x)-(y*y),2*x*y,(x*x)+(y*y))
                      else error "El 2º es > que el 1º"
-- C
p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
  where
    (l1,l2,h) = terna x y

-- Ejercicio 2
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

-- Ejercicio 3
-- A
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y) = if x >= y then (y,x) else (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
  where
    enOrden (x,y) = x<=y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
    mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

-- B
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) = if fst (ordena2(x,y)) == fst (ordena2(x,z))
                    then (x, fst (ordena2(y,z)), snd (ordena2(y,z))) else
                      if fst (ordena2(x,y)) == fst (ordena2(y,z))
                        then (y,fst (ordena2(x,z)), snd (ordena2(x,z))) else
                              (z, fst (ordena2(x,y)), snd (ordena2(x,y)))

-- C
p1_ordena3 x y z = enOrdenA (ordena3 (x,y,z))
  where
    enOrdenA (x,y,z) = x <= y && y <= z

-- Ejercicio 4
-- A
max2 :: Ord a => a -> a -> a
max2 x y = if x >= y then x else y

-- B
p1_max2 x y = mayor(x,y)
  where
    mayor(x,y) = (max2 x y == x) || (max2 x y == y)

p2_max2 x y = mayorde(x,y)
  where
    mayorde(x,y) = (max2 x y >= x) || (max2 x y >= y)

p3_max2 x y = mayorX(x,y)
  where
    mayorX(x,y) = x >= y && max2 x y == x

p4_max2 x y = mayorY(x,y)
  where
    mayorY(x,y) = y >= x && max2 x y == y

-- Ejercicio 5
entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z) = (y <= x) && (x <= z)

-- Ejercicio 6
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = (x == y) && (x == z)

-- Ejercicio 7
descomponer :: Integer -> (Integer, Integer, Integer)
descomponer x = (div x (3600), div (mod x(3600))(60), mod(mod x(3600))(60))

p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x && entre m (0,59) && entre s (0,59)
  where
    (h,m,s) = descomponer x

-- Ejercicio 8
unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) ~= x
-- Para el ejercicio 9, he cambiado == por ~=

-- Ejercicio 9
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where
    epsilon = 1/1000

-- Ejercicio 10
raíces :: Double -> Double -> Double -> (Double,Double)
raíces a b c = if (b*b)-4*a*c >= 0
                then (((-b)+sqrt((b*b)-4*a*c))/(2*a) ,((-b)-sqrt((b*b)-4*a*c))/(2*a))
                else error "Raices no reales"

p1_raíces a b c = esRaíz r1 && esRaíz r2
  where
    (r1,r2) = raíces a b c
    esRaíz r = a*r^2 + b*r + c ~= 0

p2_raíces a b c = a/=0 && ((b*b)-(4*a*c))>=0 ==> esRaíz r1 && esRaíz r2
  where
    (r1,r2) = raíces a b c
    esRaíz r = a*r^2 + b*r + c ~= 0

-- Ejercicio 11
esMultiplo :: (Integral a) => a -> a -> Bool
esMultiplo x y = mod x y == 0

-- Ejercicio 12
(==>>) :: Bool -> Bool -> Bool
x ==>> y  | x == True && y == True = True
          | x == False && y == True = True
          | x == True && y == False = False
          | x == False && y == False = True

-- Ejercicio 13
esBisiesto :: Integer -> Bool
esBisiesto x = if mod x 100 == 0 && mod x 400 == 0 && mod x 4 == 0
                then mod x 4 == 0
                else mod x 4 == 0 && mod x 100 /= 0

-- Ejercicio 14
potencia :: Integer -> Integer -> Integer
potencia x 1 = x
potencia x 0 = 1
potencia x y = x*(potencia x (y-1))

potencia' :: Integer -> Integer -> Integer
potencia' x y = if mod y 2 == 0
                then (x^(div y 2))^2
                else x*((x^(div (y-1) 2))^2)

--pot b n
-- | even n = x*x
-- | otherwise =
-- where
--   x = pot b (div n 2)

p_pot b n = n>=0 ==> potencia b n == sol && potencia' b n == sol
  where
    sol = b^n

-- Ejercicio 15
factorial :: Integer -> Integer
factorial 1 = 1
factorial 0 = 1
factorial x = x*factorial(x-1)

-- Ejercicio 16
divideA :: Integer -> Integer -> Bool
divideA x y = mod y x == 0

p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

p2_divideA x y z = x/=0 && y/=0 && z/=0 && divideA x y && divideA x z ==> divideA x (y+z)

-- Ejercicio 17
mediana :: Ord a => (a,a,a,a,a) -> a
mediana (a,b,c,d,e) | a > c = mediana(c,b,a,d,e)
                    | b > c = mediana(a,c,b,d,e)
                    | c > d = mediana(a,b,d,c,e)
                    | c > e = mediana(a,b,e,d,c)
                    | otherwise = c
