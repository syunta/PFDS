module Chapter1 where

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
member :: (Ord a) => a -> UnbalancedSet a -> Bool
member _  E            = False
member x' t@(T _ y' _) = iter x' y' t
  where
    iter x l E         = x == l
    iter x l (T a y b) =
      if x < y then iter x l a
      else iter x y b
