{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FiniteMap where

import Prelude hiding (lookup)

class FiniteMap a k v where
  empty  :: a k v
  bind   :: k -> v -> a k v -> a k v
  lookup :: k -> a k v -> Maybe v
