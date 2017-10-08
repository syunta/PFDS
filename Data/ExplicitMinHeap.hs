module Data.ExplicitMinHeap(
  ExplicitMinHeap(..),
  empty,
  isEmpty,
  insert,
  merge,
  findMin,
  deleteMin,
) where

import Data.Heap

data ExplicitMinHeap h a = E | NE a (h a)

instance Heap h => Heap (ExplicitMinHeap h) where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  findMin (NE x _) = x

  deleteMin (NE _ h) = let h' = deleteMin h in
                       NE (findMin h') h'

  insert x E        = NE x empty
  insert x (NE _ h) = let h' = merge (insert x empty) h in
                      NE (findMin h') h'

  merge h E = h
  merge E h = h
  merge (NE _ h1) (NE _ h2) = let h' = merge h1 h2 in
                              NE (findMin h') h'
