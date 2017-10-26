module Data.BatchedQueue(
  BatchedQueue(..),
  empty, isEmpty, snoc, head, tail
) where

import Prelude hiding(head, tail)
import Data.Queue

data BatchedQueue a = E | Q [a] [a] deriving (Show,Eq)

checkf :: BatchedQueue a -> BatchedQueue a
checkf (Q [] r) = Q (reverse r) []
checkf q        = q

instance Queue BatchedQueue where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  snoc (Q f r) x = checkf $ Q f (x:r)

  head (Q (x:_) _) = x

  tail (Q (_:f) r) = checkf $ Q f r
