import Test.QuickCheck

twice :: Integer -> Integer
twice x = x + x

square :: (Num a) => a -> a
square x = x * x

pythagoras :: Integer -> Integer -> Integer
pythagoras x y = square x + square y

fact :: Integer -> Integer
fact n = if n==0 then 1 else n * fact (n-1)

second :: Integer -> Integer -> Integer
second x y = y

numsFrom :: Integer -> [Integer]
numsFrom x = x : numsFrom(x+1)

f :: [Integer] -> Integer
f[] = 0
f[x] = x*10
f[x,y] = x+y
f xs = head xs

long :: [a] -> Integer
long[] = 0
long (x:xs) = 1 + long xs

succPred :: Int -> (Int, Int)
succPred x = (x+1, x-1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

p1 x y = True ==> square (x+y) == square x + square y + 2*x*y

p2 x y = x >= 0 && y >= 0 ==> abs(x*y) == abs x * abs y

{-
Una Clase de tipos es un conjunto de tipos
Num, es una clase de tipos para numeros, int, double...
Todos los tipos de la clase Num defienen las operaciones suma, resta, producto...
-}

signo :: (Ord a, Num a) => a -> a
signo x | x > 0 = 1
        | x < 0 = -1
        | otherwise = 0

reciproco :: (Eq a, Fractional a) => a -> a
reciproco x | x == 0 = error "Argumento es cero"
            | otherwise = 1/x

fun :: Double -> Double
fun x = g (2*x) + h x + z
  where
    g y = y + y
    h v = v*v
    z = 5.5

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n == 0 = []
    | n  > 0 = x : take' (n-1) xs
    | otherwise = error "Entero negativo"

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
    | n == 0 = (x:xs)
    | n > 0 = drop' (n-1) xs
    | otherwise = error "Entero negativo"

f1 :: Int -> Int -> Int -> Int
f1 x y z = x + 2*y + 3*z

esMúltiploDe :: Integer -> Integer -> Bool
esMúltiploDe x y = mod y x == 0

isEven :: Integer -> Bool
isEven x = mod x 2 == 0

data Direccion = North | South | East | West deriving (Show, Eq, Ord)

girarIzq :: Direccion -> Direccion
girarIzq North = West
girarIzq West  = South
girarIzq South = East
girarIzq East  = North

{-
instance Eq Direccion where
    North == North = True
    West  == West  = True
    East  == East  = True
    South == South = True
    _     == _     = False
-}
