module Data.BinomialHeap'(
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

data Tree a = N a [Tree a] deriving (Show,Eq)
newtype BinomialHeap a = BH [(Int, Tree a)]

link :: Ord a => (Int, Tree a) -> (Int, Tree a) -> (Int, Tree a)
link (r, t1@(N x1 c1)) (_, t2@(N x2 c2))
  | x1 <= x2  = (r+1, N x1 (t2:c1))
  | otherwise = (r+1, N x2 (t1:c2))

rank :: (Int, Tree a) -> Int
rank = fst

root :: (Int, Tree a) -> a
root (_, (N x _))  = x

tree :: BinomialHeap a -> [(Int, Tree a)]
tree (BH ts) = ts

insTree :: Ord a => (Int, Tree a) -> [(Int, Tree a)] -> [(Int, Tree a)]
insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insTree (link t t') ts'

mrg :: Ord a => [(Int, Tree a)] -> [(Int, Tree a)] -> [(Int, Tree a)]
mrg xs [] = xs
mrg [] ys = ys
mrg xs@(x:xs') ys@(y:ys')
  | rank x < rank y = x : mrg xs' ys
  | rank y < rank x = y : mrg xs ys'
  | otherwise       = insTree (link x y) (mrg xs' ys')

removeMinTree :: Ord a => [(Int, Tree a)] -> ((Int, Tree a), [(Int, Tree a)])
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
                         if root t <= root t'
                           then (t, ts)
                           else (t', t:ts')

reverseTree :: Int -> [Tree a] -> [(Int, Tree a)]
reverseTree r ts = reverseTree' r ts []

reverseTree' :: Int -> [Tree a] -> [(Int, Tree a)] -> [(Int, Tree a)]
reverseTree' _ [] ts = ts
reverseTree' r (t:ts) ts' = reverseTree' (r - 1) ts $ (r, t):ts'

instance Heap BinomialHeap where

  empty = BH []

  isEmpty (BH []) = True
  isEmpty _       = False

  insert x (BH ts) = BH $ insTree (0, (N x [])) ts

  merge (BH xs) (BH ys) = BH $ mrg xs ys

  findMin (BH ts) = let (t, _) = removeMinTree ts in root t

  deleteMin (BH ts) = let ((r, N _ ts1), ts2) = removeMinTree ts in
                      BH $ mrg (reverseTree (r-1) ts1) ts2
