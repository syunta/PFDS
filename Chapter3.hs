module Chapter3 where

import Data.LeftistHeap
import qualified Data.WeightLeftistHeap as W

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
insert' :: Int -> LeftistHeap -> LeftistHeap
insert' x E = T 1 x E E
insert' x h@(T _ y a b)
  | x <= y    = makeT x E h
  | otherwise = makeT y a (insert' x b)

-- 3.3
fromList :: [Int] -> LeftistHeap
fromList xs = fromList' $ map (\x -> insert x E) xs

fromList' :: [LeftistHeap] -> LeftistHeap
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
merge' :: W.WeightLeftistHeap -> W.WeightLeftistHeap -> W.WeightLeftistHeap
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
