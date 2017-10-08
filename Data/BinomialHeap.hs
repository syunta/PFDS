module Data.BinomialHeap(
  BinomialHeap(..),
  rank, root, tree, link, insTree, removeMinTree,
  empty,
  isEmpty,
  insert,
  merge,
  findMin,
  deleteMin,
) where

import Data.Heap

data Tree a = N Int a [Tree a] deriving (Show,Eq)
newtype BinomialHeap a = BH [Tree a]

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(N r x1 c1) t2@(N _ x2 c2)
  | x1 <= x2  = N (r+1) x1 (t2:c1)
  | otherwise = N (r+1) x2 (t1:c2)

rank :: Tree a -> Int
rank (N r _ _)  = r

root :: Tree a -> a
root (N _ x _)  = x

tree :: BinomialHeap a -> [Tree a]
tree (BH ts) = ts

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insTree (link t t') ts'

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
                              if root t <= root t'
                                then (t, ts)
                                else (t', t:ts')

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg xs [] = xs
mrg [] ys = ys
mrg xs@(x:xs') ys@(y:ys')
  | rank x < rank y = x : mrg xs' ys
  | rank y < rank x = y : mrg xs ys'
  | otherwise       = insTree (link x y) (mrg xs' ys')

instance Heap BinomialHeap where

  empty = BH []

  isEmpty (BH []) = True
  isEmpty _       = False

  insert x (BH ts) = BH $ insTree (N 0 x []) ts

  merge (BH xs) (BH ys) = BH $ mrg xs ys

  findMin (BH ts) = let (t, _) = removeMinTree ts in root t

  deleteMin (BH ts) = let (N _ _ ts1, ts2) = removeMinTree ts in
                      BH $ mrg (reverse ts1) ts2
