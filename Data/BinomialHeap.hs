module Data.BinomialHeap where

data Tree = N Int Int [Tree] deriving (Show,Eq)

type BinomialHeap = [Tree]

link :: Tree -> Tree -> Tree
link t1@(N r x1 c1) t2@(N _ x2 c2)
  | x1 <= x2  = N (r+1) x1 (t2:c1)
  | otherwise = N (r+1) x2 (t1:c2)

rank :: Tree -> Int
rank (N r _ _)  = r

root :: Tree -> Int
root (N _ x _)  = x

insTree :: Tree -> BinomialHeap -> BinomialHeap
insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t:ts
  | otherwise        = insTree (link t t') ts'

insert :: Int -> BinomialHeap -> BinomialHeap
insert x ts = insTree (N 0 x []) ts

merge :: BinomialHeap -> BinomialHeap -> BinomialHeap
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | rank x < rank y = x : merge xs' ys
  | rank y < rank x = y : merge xs ys'
  | otherwise       = insTree (link x y) (merge xs' ys')

removeMinTree :: BinomialHeap -> (Tree, BinomialHeap)
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
                         if root t <= root t'
                           then (t, ts)
                           else (t', t:ts')

findMin :: BinomialHeap -> Int
findMin ts = let (t, _) = removeMinTree ts in root t

deleteMin :: BinomialHeap -> BinomialHeap
deleteMin ts = let (N _ x ts1, ts2) = removeMinTree ts in
                 merge (reverse ts1) ts2
