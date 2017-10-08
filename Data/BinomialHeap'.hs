module Data.BinomialHeap' where

data Tree = N Int [Tree] deriving (Show,Eq)

type BinomialHeap = [(Int, Tree)]

link :: (Int, Tree) -> (Int, Tree) -> (Int, Tree)
link (r, t1@(N x1 c1)) (_, t2@(N x2 c2))
  | x1 <= x2  = (r+1, N x1 (t2:c1))
  | otherwise = (r+1, N x2 (t1:c2))

rank :: (Int, Tree) -> Int
rank = fst

root :: (Int, Tree) -> Int
root (_, (N x _))  = x

insTree :: (Int, Tree) -> BinomialHeap -> BinomialHeap
insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insTree (link t t') ts'

insert :: Int -> BinomialHeap -> BinomialHeap
insert x ts = insTree (0, (N x [])) ts

merge :: BinomialHeap -> BinomialHeap -> BinomialHeap
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | rank x < rank y = x : merge xs' ys
  | rank y < rank x = y : merge xs ys'
  | otherwise       = insTree (link x y) (merge xs' ys')

removeMinTree :: BinomialHeap -> ((Int, Tree), BinomialHeap)
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
                         if root t <= root t'
                           then (t, ts)
                           else (t', t:ts')

findMin :: BinomialHeap -> Int
findMin ts = let (t, _) = removeMinTree ts in root t

deleteMin :: BinomialHeap -> BinomialHeap
deleteMin ts = let ((r, N _ ts1), ts2) = removeMinTree ts in
                 merge (reverseTree (r-1) ts1) ts2

reverseTree :: Int -> [Tree] -> [(Int, Tree)]
reverseTree r ts = reverseTree' r ts []

reverseTree' :: Int -> [Tree] -> [(Int, Tree)] -> [(Int, Tree)]
reverseTree' _ [] ts = ts
reverseTree' r (t:ts) ts' = reverseTree' (r - 1) ts $ (r, t):ts'
