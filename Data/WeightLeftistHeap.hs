module Data.WeightLeftistHeap(
  WeightLeftistHeap(..),
  size, makeT,
  empty,
  isEmpty,
  insert,
  merge,
  findMin,
  deleteMin,
) where

import Data.Heap

data WeightLeftistHeap a = E | T Int a (WeightLeftistHeap a) (WeightLeftistHeap a) deriving (Show,Eq)

size :: WeightLeftistHeap a -> Int
size E           = 0
size (T s _ _ _) = s

makeT :: a -> WeightLeftistHeap a -> WeightLeftistHeap a -> WeightLeftistHeap a
makeT x a b
  | sa >= sb  = T (1+(sa)+(sb)) x a b
  | otherwise = T (1+(sa)+(sb)) x b a
  where sa = size a
        sb = size b

instance Heap WeightLeftistHeap where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge E h = h
  merge h E = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    | x <= y    = makeT x a1 (merge b1 h2)
    | otherwise = makeT y a2 (merge h1 b2)

  insert x h = merge (T 1 x E E) h

  findMin (T _ x _ _) = x

  deleteMin (T _ _ a b) = merge a b
