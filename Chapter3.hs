module Chapter3 where

import Data.Heap
import Data.LeftistHeap
import qualified Data.WeightLeftistHeap as W
import qualified Data.BinomialHeap as B

-- 3.1
{-
       rank element
          3 1
       /       \
    2 3          2 2
  /     \      /     \
1 4    1 5    1 6    1 7

釣り合った二分木のとき,右スパインが最も長くなる.
このとき木の高さ(右スパイン)は, log(n + 1)

-}

-- 3.2
insert' :: (Ord a, Show a) => a -> LeftistHeap a -> LeftistHeap a
insert' x E = T 1 x E E
insert' x h@(T _ y a b)
  | x <= y    = makeT x E h
  | otherwise = makeT y a (insert' x b)

-- 3.3
fromList :: Ord a => [a] -> LeftistHeap a
fromList xs = fromList' $ map (\x -> insert x E) xs

fromList' :: Ord a => [LeftistHeap a] -> LeftistHeap a
fromList' [x] = x
fromList' xs = fromList' $ map2 merge E xs

map2 :: (a -> a -> b) -> a -> [a] -> [b]
map2 _ _ []       = []
map2 f a [x]      = [f a x]
map2 f a (x:y:xs) = f x y : map2 f a xs

-- log2(n) * log2(n) は常に log2(n) * log2(n) <= n を満たすので、fromList は O(n) 時間しかかからない

-- 3.4

-- (a)
{-
       size element
          7 1
       /       \
    3 3          3 2
  /     \      /     \
1 4    1 5    1 6    1 7

釣り合った二分木のとき,右スパインが最も長くなる.
このとき木の高さ(右スパイン)は, log(n + 1)

-}

-- (b)
-- Data.WeightLeftistHeap

-- (c)
merge' :: Ord a => W.WeightLeftistHeap a -> W.WeightLeftistHeap a -> W.WeightLeftistHeap a
merge' W.E h = h
merge' h W.E = h
merge' h1@(W.T _ x a1 b1) h2@(W.T _ y a2 b2)
  | x <= y    = let a  = a1
                    b  = merge' b1 h2
                    sa = W.size a
                    sb = W.size b in
                  if sa >= sb
                    then W.T (1+(sa)+(sb)) x a b
                    else W.T (1+(sa)+(sb)) x b a
  | otherwise = let a  = a2
                    b  = merge' h1 b2
                    sa = W.size a
                    sb = W.size b in
                  if sa >= sb
                    then W.T (1+(sa)+(sb)) y a b
                    else W.T (1+(sa)+(sb)) y b a

-- (d)
{-

遅延評価時の利点は merge' の再帰処理を待たずに merge' しなかった方の子へのアクセスができるので早くなる?
(遅延)並列実行時の利点は結果が常に同じになる?

-}

-- 3.5
findMin' :: Ord a => B.BinomialHeap a -> a
findMin' (B.BH [t])    = B.root t
findMin' (B.BH (t:ts)) = let r1 = B.root t
                             r2 = findMin' (B.BH ts) in
                         if r1 <= r2 then r1 else r2

-- 3.6
-- Data.BinomialHeap'

-- 3.7
-- Data.ExplicitMinHeap
