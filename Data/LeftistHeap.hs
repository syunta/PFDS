module Data.LeftistHeap(
  LeftistHeap(..),
  rank, makeT,
  empty,
  isEmpty,
  insert,
  merge,
  findMin,
  deleteMin,
) where

import Data.Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving (Show,Eq)

rank :: LeftistHeap a -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b
  | rank a >= rank b = T (rank b+1) x a b
  | otherwise        = T (rank a+1) x b a

instance Heap LeftistHeap where

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

  deleteMin (T _ x a b) = merge a b
