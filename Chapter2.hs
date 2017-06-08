{-# LANGUAGE MultiParamTypeClasses,  FlexibleInstances, InstanceSigs #-}

module Chapter2 where

import Data.FiniteMap
import Prelude hiding (lookup)

-- 2.1

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_:xs') = xs : suffixes xs'

{-

普通の線形末尾再帰なので O(n) 時間で生成できる.
生成されるリストは下図のようになるため O(n) の空間で表現できる.

(1,*) -> (2,*) -> (3,*) -> (4,*) -> ()
 ^        ^        ^        ^
 |        |        |        |        ()
 |        |        |        |        ^
 |        |        |        |        |
(*,*) -> (*,*) -> (*,*) -> (*,*) -> (*,*) -> ()

-}

suffixes' :: [a] -> [[a]]
suffixes' []     = [[]]
suffixes' (x:xs) = (x:xs) : suffixes' xs

{-

このような実装の場合, 生成されるリストは下図のようになるため O(n) の空間で表現できない.

(1,*) -> (2,*) -> (3,*) -> (4,*) -> ()
          ^        ^        ^        ^
          |        |        |        |
(1,*) ----+        |        |        |
 ^                 |        |        |
 |                 |        |        |
 |       (2,*) ----+        |        |
 |        ^                 |        |
 |        |       (3,*) ----+        |
 |        |        ^                 |
 |        |        |       (4,*) ----+
 |        |        |        |
(*,*) -> (*,*) -> (*,*) -> (*,*) -> ()

-}

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)
                       deriving (Show, Eq)

-- 2.2
-- member の結果が False になる場合は d + 1 となるが, True になる場合でも d + 1 なのでケースバイケースと思われる.
member :: (Ord a) => a -> UnbalancedSet a -> Bool
member _  E            = False
member x t@(T _ y' _) = iter y' t
  where
    iter l E = x == l
    iter l (T a y b)
      | x < y     = iter l a
      | otherwise = iter y b

-- 2.3
insert :: (Ord a) => a -> UnbalancedSet a -> UnbalancedSet a
insert x E = T E x E
insert x s = iter s id
  where
    iter E f = f $ T E x E
    iter (T a y b) f
      | x < y  = iter a (\t -> f $ T t y b)
      | x > y  = iter b (\t -> f $ T a y t)
      | x == y = s

-- 2.4
insert' :: (Ord a) => a -> UnbalancedSet a -> UnbalancedSet a
insert' x E = T E x E
insert' x s@(T _ y' _) = iter y' s id
  where
    iter l E f
      | x == l    = s
      | otherwise = f $ T E x E
    iter l (T a y b) f
      | x < y     = iter y a (\t -> f $ T t y b)
      | otherwise = iter l b (\t -> f $ T a y t)

-- 2.5
-- (a)
complete :: (Ord a) => a -> Int -> UnbalancedSet a
complete _ 0 = E
complete x n = T t x t
  where t = complete x (n-1)

-- (b)
create :: (Ord a) => a -> Int -> UnbalancedSet a
create _ 0 = E
create x 1 = T E x E
create x m
  | m `mod` 2 == 0 = let (t1, t2) = create2 x ((m `div` 2) - 1) in
                     T t1 x t2
  | otherwise      = let t = create x (m `div` 2) in
                     T t x t

create2 :: (Ord a) => a -> Int -> (UnbalancedSet a, UnbalancedSet a)
create2 x m = (create x m, create x (m+1))

-- 2.6

data UnbalancedMap k v = Empty | M (UnbalancedMap k v) (k, v) (UnbalancedMap k v)
                         deriving (Show, Eq)

instance Ord k => FiniteMap UnbalancedMap k v where

  empty :: UnbalancedMap k v
  empty = Empty

  bind :: k -> v -> UnbalancedMap k v -> UnbalancedMap k v
  bind k v Empty = M Empty (k,v) Empty
  bind k v (M l (a, b) r)
    | k < a  = M (bind k v l) (a, b) r
    | k > a  = M l (a, b) (bind k v r)
    | k == a = M l (k, v) r

  lookup :: k -> UnbalancedMap k v -> Maybe v
  lookup _ Empty = Nothing
  lookup k (M l (a, b) r)
    | k < a  = lookup k l
    | k > a  = lookup k r
    | k == a = Just b
