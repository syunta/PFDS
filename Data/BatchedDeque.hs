module Data.BatchedDeque(
  BatchedDeque(..),
  empty, isEmpty, cons, head, tail, snoc, last, init
) where

import Prelude hiding(head, tail, last, init)
import Data.Deque

data BatchedDeque a = E | Q [a] [a] deriving (Show,Eq)

checkf :: BatchedDeque a -> BatchedDeque a
checkf (Q [] r) = let (f', r') = splitAt (length r `div` 2) r in Q (reverse r') f'
checkf (Q f []) = let (f', r') = splitAt (length f `div` 2) f in Q f' (reverse r')
checkf q        = q

instance Deque BatchedDeque where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  cons q x = let Q f r = checkf q in Q (x:f) r

  head (Q (x:_) _) = x

  tail q = let Q (_:f) r = checkf q in Q f r

  snoc q x = let Q f r = checkf q in Q f (x:r)

  last (Q _ (x:_)) = x

  init q = let Q f (_:r) = checkf q in Q f r
