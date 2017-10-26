module Chapter4 where

-- 4.2

insertionSort :: Ord a => [a] -> [a]
insertionSort xs = insertionSort' xs []

insertionSort' :: Ord a => [a] -> [a] -> [a]
insertionSort' []     ys = ys
insertionSort' (x:xs) ys = insertionSort' xs $ insertElement x ys

insertElement :: Ord a => a -> [a] -> [a]
insertElement a [] = [a]
insertElement a s@(x:xs)
  | a > x     = x : insertElement a xs
  | otherwise = a : s
