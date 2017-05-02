-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: LUQUE FERNANDEZ, DAVID
-- Fecha de entrega: 09 | 11 | 2015
--
-- Relación de Ejercicios 2. Ejercicios resueltos: Del 1 al 26 y el 40.
--
-------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List
import Numeric(showFFloat)

-- EJERCICIO 1
data Direction = North | South | East | West deriving (Eq, Ord, Enum, Show)

(<<) :: Direction -> Direction -> Bool
x << y = fromEnum x < fromEnum y

p_menor x y = (x < y) == (x << y)
instance Arbitrary Direction where
  arbitrary = do
    n <- choose (0,3)
    return $ toEnum n

-- EJERCICIO 2
maximoYresto :: Ord a => [a] -> (a,[a])
maximoYresto (x:xs) = (maximum(x:xs),filter (/= maximum(x:xs)) (x:xs))

-- EJERCICIO 3
reparte :: [a] -> ([a],[a])
reparte [] = ([],[])
reparte (x:y:xs) = (lista1 x xs,lista2 y (drop 1 xs))
  where
    lista1 z [] = [z]
    lista1 z zs = [z] ++ lista1 (head zs) (drop 2 zs)
    lista2 w [] = [w]
    lista2 w ws = [w] ++ lista2 (head ws) (drop 2 ws)
{-
reparte (x:xs) = if(mod (length (x:xs)) 2 == 0)
  then (take result (x:xs),drop result (x:xs))
  else (take (result+1) (x:xs),drop (result+1) (x:xs))
  where
    result = div (length(x:xs)) 2
-}

-- EJERCICIO 4
distintos :: Ord a => [a] -> Bool
distintos [] = True
distintos (x:xs) = compara (x:xs)
  where
    compara [] = True
    compara (z:zs) = if(or (map (==z) (zs)))
      then False
      else compara zs

-- EJERCICIO 5
replicate' :: Int -> a -> [a]
replicate' y z = [ z | x <- [1..y]]

p_replicate' n x = n >= 0 && n <= 1000 ==>
  length (filter (==x) xs) == n
  && length (filter (/=x) xs) == 0
  where
    xs = replicate' n x

-- EJERCICIO 6
divideA :: Integer -> Integer -> Bool
divideA x y = mod y x == 0

divisores :: Integer -> [Integer]
divisores z = [x | x <- [1..z], (divideA x z)]

divisores' :: Integer -> [Integer]
divisores' z = if (z < 0)
  then [x | x <- [(z)..(-1)], (divideA x (z))]
                ++ [x | x <- [1..(-z)], (divideA x (-z))]
  else [x | x <- [1..z], (divideA x z)]

-- EJERCICIO 7
mcd :: Integer -> Integer -> Integer
mcd x y = mcd' (abs x) (abs y)
 where
    mcd' z t = maximum [ if(x==y) then x else 1 | x <- divisores z , y <- divisores t]

p_mcd x y z = x/=0 && y/=0 && z/=0  ==> mcd(z*x) (z*y) == (abs z) * (mcd x y)

mcm :: Integer -> Integer -> Integer
mcm z t = div (z*t) (mcd z t)

-- EJERCICIO 8
esPrimo :: Integer -> Bool
esPrimo z = [ x | x <- [1..z] , (divideA x z)] == [1,z]

primosHasta :: Integer -> [Integer]
primosHasta z = [x | x <- [1..z], (esPrimo x)]

primosHasta' :: Integer -> [Integer]
primosHasta' z = filter (esPrimo) [1..z]

p1_primos x = primosHasta x == primosHasta' x

-- EJERCICIO 9
pares :: Integer -> [(Integer,Integer)]
pares z = [(x,y) | x <- [1..(div z 2)] , y <- [1..z] , (esPrimo x) , (esPrimo y), (x+y==z)]

goldbach :: Integer -> Bool
goldbach z = z > 2 && not (null (pares z))

goldbachHasta :: Integer -> Bool
goldbachHasta z = and [goldbach x | x <- [4,6..z]]

triples :: Integer -> [(Integer,Integer,Integer)]
triples t = [(x,y,z) | x <- [1..(div t 3)] , y <- [1..(div t 3)] , z <- [1..t], (esPrimo x), (esPrimo y), (esPrimo z), (x+y+z==t)]

goldbachDebil :: Integer -> Bool
goldbachDebil z = (z > 5 && not (null (triples z)))

goldbachDebilHasta :: Integer -> Bool
goldbachDebilHasta z = and [goldbachDebil x | x <- [7,9..z]]

-- EJERCICIO 10
esPerfecto :: Integer -> Bool
esPerfecto z = sum ([x | x <- [1..(z-1)], (divideA x z)]) == z

perfectosMenoresQue :: Integer -> [Integer]
perfectosMenoresQue z = [x | x <- [1..z], (esPerfecto x)]

-- EJERCICIO 11
take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [0..(n-1)] xs]

drop' :: Int -> [a] -> [a]
drop' n xs = [ x | (p,x) <- zip [0..(length xs)] xs, p > (n-1)  ]

p_takedrop n xs = n >=0 ==> (take' n xs) ++ (drop' n xs) == xs

-- EJERCICIO 12
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

concat'' :: [[a]] -> [a]
concat'' (xs:xss) = [z | y <- (xs : xss), z <- y]

-- EJERCICIO 13
desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [x <=y | (x,y) <- zip xs (tail xs)]
-- Comprueba que la lista esta ordenada.

-- EJERCICIO 14
inserta :: (Ord a) => a -> [a] -> [a]
inserta x xs = (takeWhile (<x) xs) ++ [x] ++ (dropWhile (<x) xs )

inserta' :: (Ord a) => a -> [a] -> [a]
inserta' y (x:xs) = if ([] == (xs))
  then if (y < x) then [y] ++ [x] else [x] ++ [y]
  else if (x > y)
    then [y] ++ (x:xs)
    else [x] ++ (inserta' y (xs))

p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs
--ordena (x:xs) = if (xs == [])
--  then inserta x []
--  else inserta x (ordena xs)

p_ordena xs = desconocida (ordena xs)

ordenaInduc :: (Ord a) => [a] -> ([a], Bool)
ordenaInduc [] = ([],True)
ordenaInduc (x:xs) = (ordena(x:xs) , desconocida(ordena (x:xs)))

-- EJERCICIO 15
--iterate :: (a -> a) -> a -> [a]
--iterate f x = x : iterate f (f x)
geométrica :: Integer -> Integer ->  [Integer]
geométrica x y = iterate (*y) x

p1_geométrica x r = x>0 && r>0 ==> and [ div z y == r | (y,z) <- zip xs (tail xs) ]
  where xs = take 100 (geométrica x r)

múltiplosDe :: Integer -> [Integer]
múltiplosDe x = iterate (+x) 0

potenciasDe :: Integer -> [Integer]
potenciasDe x = iterate (*x) 1

-- EJERCICIO 16
múltiplosDe' :: Integer -> [Integer]
múltiplosDe' x = iterate (+x) x

primerComún :: Ord a => [a] -> [a] -> a
primerComún (x:xs) [] = error "No existe"
primerComún [] (y:ys) = error "No existe"
primerComún (x:xs) (y:ys) = if(x==y)
  then x
  else if (x < y)
    then primerComún (xs) (y:ys)
    else primerComún (x:xs) (ys)

mcm' :: Integer -> Integer -> Integer
mcm' x y = primerComún (múltiplosDe' x) (múltiplosDe' y)

p_mcm x y = x>=0 && y>=0 ==> mcm' x y == lcm x y

-- EJERCICIO 17
primerComúnDeTres :: Ord a => [a] -> [a] -> [a] -> a
primerComúnDeTres (x:xs) (y:ys) [] = error "No existe"
primerComúnDeTres (x:xs) [] (z:zs) = error "No existe"
primerComúnDeTres [] (y:ys) (z:zs) = error "No existe"
primerComúnDeTres (x:xs) (y:ys) (z:zs) = if (x == y && x == z)
  then x
  else if (x < y || x < z)
    then primerComúnDeTres (xs) (y:ys) (z:zs)
    else if (y < x || y < z)
      then primerComúnDeTres (x:xs) (ys) (z:zs)
      else if (z < x || z < y)
        then primerComúnDeTres (x:xs) (y:ys) (zs)
        else error "No existe"

-- EJERCICIO 18
factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
  where
    fp x d
      | x' < d = [x]
      | r == 0 = d : fp x' d
      | otherwise = fp x (d+1)
      where (x',r) = divMod x d -- cociente y resto

-- EJERCICIO 19
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] [] = []
mezcla [] (y:ys) = (y:ys)
mezcla (x:xs) [] = (x:xs)
mezcla (x:xs) (y:ys) = if (x == y)
  then [x] ++ mezcla (xs) (ys)
  else if (x < y)
    then [x] ++ mezcla (xs) (y:ys)
    else [y] ++ mezcla (x:xs) (ys)

mcm'' :: Integer -> Integer -> Integer
mcm'' x y = product (mezcla (factPrimos x) (factPrimos y))

p_mcm' x y = x>=0 && y>=0 ==> mcm'' x y == lcm x y

-- EJERCICIO 20
mezc' :: Ord a => [a] -> [a] -> [a]
mezc' [] [] = []
mezc' (x:xs) [] = []
mezc' [] (y:ys) = []
mezc' (x:xs) (y:ys) = if (x == y)
  then [x] ++ mezc' (xs) (ys)
  else if (x < y)
    then mezc' (xs) (y:ys)
    else mezc' (x:xs) (ys)

mcd' :: Integer -> Integer -> Integer
mcd' x y = product (mezc' (factPrimos x) (factPrimos y))

p_mcd' x y = x>0 && y>0 ==> mcd' x y == gcd x y

-- EJERCICIO 21
p_neutroDer x = [] ++ x == x ++ []

-- EJERCICIO 22
p_asociativa x y = x ++ y == y ++ x

-- EJERCICIO 23
nub' :: [Integer] -> [Integer]
nub' [] = []
nub' (x:xs) = [x] ++ nub' (filter (/=x) xs)

p_nub' xs = nub xs == nub' xs

p_sinRepes xs = distintos (nub' xs)

todosEn :: (Eq a) => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinRepes' xs = distintos (nub' xs)

-- EJERCICIO 24
binarios :: Integer -> [[Char]]
binarios 0 = [""]
--binarios x = map ("0"++) (binarios(x-1)) ++ map ("1"++) (binarios(x-1))

binarios x = concat [ map (x:) xs | x <- "01" ]
  where
    xs = binarios (x-1)

long :: [a] -> Integer
long xs = fromIntegral (length xs)

p_binarios n = n>=0 && n<=10 ==> long xss == 2^n && distintos xss && all (`todosEn` "01") xss
  where xss = binarios n

-- EJERCICIO 25
varRep :: Integer -> [a] -> [[a]]
varRep 0 hs = [[]]
varRep x ys = aux ys (varRep (x-1) ys)
  where
    aux hs zs = concat [ map (h:) zs |   h <- hs]

p_varRep m xs = m >=0 && m <= 5 && n <= 5 && distintos xs ==> long vss == n^m && distintos vss && all (`todosEn` xs) vss
    where
      vss = varRep m xs
      n = long xs

simbolosMorse :: Integer
simbolosMorse = long (varRep 5 "-.")

-- EJERCICIO 26

var :: (Eq a) => Int -> [a] -> [[a]]
var 0 hs = [[]]
var x ys = filter (\xs -> length xs == x ) (aux ys (var (x-1) ys))
  where
    aux hs zss = concat [map (\zs -> if elem h zs then zs else h:zs) zss | h <- hs]

--p_var m xs = n<=5 && distintos xs && m>=0 && m<=n ==> long vss == fact n `div` fact (n-m) && distintos vss && all distintos vss && all (`todosEn` xs) vss
  --where
    --vss = var m xs
    --n = long xs
    --fact :: Integer -> Integer
    --fact x = product [1..x]

-- EJERCICIO 40
type Fila = [Double]
data Matriz = M Int Int [Fila] deriving Eq

m1 :: Matriz
m1 = M 2 3 [[1/3,-1/2, 1/5], [1/8, 3/7, 1/4]]

instance Show Matriz where
  show (M f c fs) =
    unlines . map (("| "++) . (++" |") . unwords . map rellena) $ fs
      where
        ancho = 8 -- ancho a ocupar para cada columna
        decimales = 2 -- nº de decimales
        rellena n
          | l >= ancho = replicate ancho '*'
          | otherwise = replicate (ancho-l) ' ' ++ xs
          where
            xs = showFFloat (Just decimales) n ""
            l = length xs

esMatriz :: Matriz -> Bool
esMatriz (M f c fs) = length fs == f && and (map (\xs -> length xs == c) fs)

sumaF :: Fila -> Fila -> Fila
sumaF xs ys = if (length xs /= length ys)
  then error "Las dos filas tienen que tener el mismo número de elementos"
  else map (\(h,z) -> h + z) (zip xs ys)


sumaM :: Matriz -> Matriz -> Matriz
sumaM (M f c xs) (M h z ys) = if f == h && c == z
  then (M f c (map (\(p,k) -> sumaF p k) (zip xs ys)))
  else error "Matrices no sumables"

restaM :: Matriz -> Matriz -> Matriz
restaM (M f c xs) (M h z ys) = if f == h && c == z
  then (M f c (map (\(p,k) -> diferencia p k) (zip xs ys)))
  else error "Matrices no sumables"
    where
      diferencia p k = map (\(i,j) -> i-j) (zip p k)
