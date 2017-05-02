module MaxiphobicHeapDemo where

import DataStructures.Heap.MaxiphobicHeap
import Data.List(nub,(\\))
import DataStructures.Graphics.DrawTrees
import DataStructures.Util.Random
import Test.QuickCheck


drawHeap :: (Show a) => Heap a -> IO ()
drawHeap = drawOn "MaxiphobicHeap.png"

outlineHeap :: Heap a -> IO ()
outlineHeap = outlineOn "MaxiphobicHeap.png"

drawCharHeap :: String -> IO ()
drawCharHeap xs = drawOnWith "MaxiphobicHeap.png" (\k -> [k]) (mkHeap xs)

drawIntHeap :: Heap Int -> IO ()
drawIntHeap h = drawOnWith "MaxiphobicHeap.png" (\k -> show k) h

randomHeap :: Int -> Seed -> Heap Int
randomHeap sz seed = mkHeap (take sz . nub . randoms $ seed)

demo1 sz seed = outlineHeap (randomHeap sz seed)
-- *MaxiphobicHeapDemos> demo1 50 2552

demo2 xs = drawHeap (mkHeap xs)
-- *MaxiphobicHeapDemos> demo2 [3,4,3,2,5,6,4,3]

demo3 = drawCharHeap "murcielago"
-- *MaxiphobicHeapDemos> demo3



h1 = foldl (flip insert) empty [4,2,3,1,2,4,7,1]
-- *MaxiphobicHeapDemos> drawHeap h1

toHeap xs = foldr insert empty xs
-- *MaxiphobicHeapDemos> drawHeap (toHeap [4,3,5,6,4,8,9,8,7])

toList h
 | isEmpty  h = []
 | otherwise  = minElem h : toList (delMin h)

heapSort :: Ord a => [a] -> [a]
heapSort = toList . toHeap

-- *MaxiphobicHeapDemos> heapSort [4,2,3,1,2,4,7,1]
-- [1,1,2,2,3,4,4,7]



type T = Char

test = quickCheck (p1 :: [T] -> Bool)

p1 xs = isSorted ys && null (xs\\ys) && null(ys\\xs)
 where
  ys = heapSort xs

isSorted :: (Ord a) => [a] -> Bool
isSorted []       =  True
isSorted [x]      =  True
isSorted (x:y:zs) =  x<=y && isSorted (y:zs)

-- *MaxiphobicHeapDemos> test
