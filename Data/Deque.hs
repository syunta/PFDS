module Data.Deque where

import Prelude hiding(head, tail)

class Deque d where
  empty  :: d a
  isEmpty :: d a -> Bool

  cons :: d a -> a -> d a
  head :: d a -> a
  tail :: d a -> d a

  snoc :: d a -> a -> d a
  last :: d a -> a
  init :: d a -> d a
