module Chapter2 where

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

empty :: (Ord a) => UnbalancedSet a
empty = E

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
