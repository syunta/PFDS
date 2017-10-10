module Chapter3 where

import Data.Heap
import Data.LeftistHeap
import qualified Data.WeightLeftistHeap as W
import qualified Data.BinomialHeap as B
import qualified Data.RedBlackSet as R

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

-- 3.8
{-

最短の経路が黒のみのノードであることに対し、同数の黒ノードが赤ノードと交互に並ぶのが最長の経路になる2進木なので、せいぜい 2log n

-}

-- 3.9
{-
fromOrdList xs = foldl (flip insert) empty xs
のように実装すると O(nlogn) になってしまう
-}
fromOrdList :: Ord a => [a] -> R.RedBlackSet a
fromOrdList xs = fromOrdList' (length xs) R.B  xs

fromOrdList' :: Ord a => Int -> R.Color -> [a] -> R.RedBlackSet a
fromOrdList' _ _ []    = R.empty
fromOrdList' _ R.B [x] = R.T R.B R.empty x R.empty
fromOrdList' _ R.R [x] = R.T R.R R.empty x R.empty
fromOrdList' n R.B xs  = R.T R.B l (xs !! m) r
                         where m = n `div` 2
                               l = fromOrdList' m R.R $ take m xs
                               r = fromOrdList' (m+1) R.R $ drop (m+1) xs
fromOrdList' n R.R xs  = R.T R.R l (xs !! m) r
                         where m = n `div` 2
                               l = fromOrdList' m R.B $ take m xs
                               r = fromOrdList' (m+1) R.B $ drop (m+1) xs

-- 3.10
-- (a)
lbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
lbalance R.B (R.T R.R (R.T R.R a x b) y c) z d = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
lbalance R.B (R.T R.R a x (R.T R.R b y c)) z d = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
lbalance c a x b = R.T c a x b

rbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
rbalance R.B a x (R.T R.R (R.T R.R b y c) z d) = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
rbalance R.B a x (R.T R.R b y (R.T R.R c z d)) = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
rbalance c a x b = R.T c a x b

insert'' :: (Ord a) => a ->  R.RedBlackSet a ->  R.RedBlackSet a
insert'' x s = let (R.T _ a y b) = ins s in R.T R.B a y b
               where ins R.E = R.T R.R R.E x R.E
                     ins s'@(R.T c a y b)
                       | x < y     = lbalance c (ins a) y b
                       | x > y     = rbalance c a y (ins b)
                       | otherwise = s'
-- (b)
insert''' :: (Ord a) => a ->  R.RedBlackSet a ->  R.RedBlackSet a
insert''' x s = let (R.T _ a y b) = ins s in R.T R.B a y b
                where ins R.E = R.T R.R R.E x R.E
                      ins s'@(R.T c a y b)
                        | x < y     = case a of
                                        R.E -> R.T c (ins a) y b
                                        R.T _ _ z _ | x < z -> llbalance c (ins a) y b
                                                    | x > z -> lrbalance c (ins a) y b
                        | x > y     = case b of
                                        R.E -> R.T c a y (ins b)
                                        R.T _ _ z _ | x < z -> rlbalance c a y (ins b)
                                                    | x > z -> rrbalance c a y (ins b)
                        | otherwise = s'

llbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
llbalance R.B (R.T R.R (R.T R.R a x b) y c) z d = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
llbalance c a x b = R.T c a x b

lrbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
lrbalance R.B (R.T R.R a x (R.T R.R b y c)) z d = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
lrbalance c a x b = R.T c a x b

rlbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
rlbalance R.B a x (R.T R.R (R.T R.R b y c) z d) = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
rlbalance c a x b = R.T c a x b

rrbalance :: R.Color -> R.RedBlackSet a -> a -> R.RedBlackSet a -> R.RedBlackSet a
rrbalance R.B a x (R.T R.R b y (R.T R.R c z d)) = R.T R.R (R.T R.B a x b) y (R.T R.B c z d)
rrbalance c a x b = R.T c a x b
