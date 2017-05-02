-------------------------------------------------------------------------------
-- Student's name: <<<write your name here>>>
--
-- Maxiphobic Heaps
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Heap.MaxiphobicHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge
  , mkHeap
  ) where

import Test.QuickCheck

data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements in tree rooted at node
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w

singleton :: a -> Heap a
singleton x  = Node x 1 Empty Empty

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h -- Crea un montículo de un elemento y lo mezcla con el original

minElem :: Heap a -> a
minElem Empty          = error "minElem on empty heap"
minElem (Node x _ _ _) = x -- El mínimo monticulo es la raiz del arbol

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty            = error "delMin on empty heap"
delMin (Node _ _ lh rh) = merge lh rh -- Elimina la raíz (el menor) y mezcla los hijos

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'   = h'
merge h    Empty = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x < x'          = Node x (w+w') monticulo1 monticulo2
 | otherwise       = merge h' h
 where
  monticulo1
    | ((size lh <= size rh)&&(size lh <= w')) = lh
    | ((size rh <= size lh)&&(size rh <= w')) = rh
    | otherwise = h'
  monticulo2
    | ((size lh <= size rh)&&(size lh <= w')) = merge rh h'
    | ((size rh <= size lh)&&(size rh <= w')) = merge lh h'
    | otherwise = merge lh rh

-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    mergeLoop [h]  = h
    mergeLoop hs   = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbritray Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkHeap xs)
