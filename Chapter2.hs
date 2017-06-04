module Chapter1 where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : (suffixes . tail) xs

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
