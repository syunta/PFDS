module Chapter4 where

-- 4.2

insertion_sort :: Ord a => [a] -> [a]
insertion_sort xs = insertion_sort' xs []

insertion_sort' :: Ord a => [a] -> [a] -> [a]
insertion_sort' []     ys = ys
insertion_sort' (x:xs) ys = insertion_sort' xs $ insert_element x ys

insert_element :: Ord a => a -> [a] -> [a]
insert_element a [] = [a]
insert_element a s@(x:xs)
  | a > x     = x : insert_element a xs
  | otherwise = a : s
