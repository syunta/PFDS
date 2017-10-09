{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.RedBlackSet(
  RedBlackSet(..),
  Color(..),
  empty, insert, member
) where

import Data.Set

data Color = R | B deriving (Show, Eq)
data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a) deriving (Show, Eq)

balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c a x b                     = T c a x b

instance Ord a => Set RedBlackSet a where

  empty = E

  member _ E = False
  member x (T _ a y b)
    | x < y     = member x a
    | x > y     = member x b
    | otherwise = True

  insert x s = let (T _ a y b) = ins s in T B a y b
    where ins E = T R E x E
          ins s'@(T c a y b)
            | x < y     = balance c (ins a) y b
            | x > y     = balance c a y (ins b)
            | otherwise = s'
