module Chapter3 where

import Data.LeftistHeap

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
