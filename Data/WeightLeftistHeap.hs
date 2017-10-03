module Data.WeightLeftistHeap where

data WeightLeftistHeap = E | T Int Int WeightLeftistHeap WeightLeftistHeap deriving (Show,Eq)

merge :: WeightLeftistHeap -> WeightLeftistHeap -> WeightLeftistHeap
merge E h = h
merge h E = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

size :: WeightLeftistHeap -> Int
size E           = 0
size (T s _ _ _) = s

makeT :: Int -> WeightLeftistHeap -> WeightLeftistHeap -> WeightLeftistHeap
makeT x a b
  | sa >= sb  = T (1+(sa)+(sb)) x a b
  | otherwise = T (1+(sa)+(sb)) x b a
  where sa = size a
        sb = size b

insert :: Int -> WeightLeftistHeap -> WeightLeftistHeap
insert x h = merge (T 1 x E E) h

findMin :: WeightLeftistHeap -> Int
findMin (T _ x _ _) = x

deleteMin :: WeightLeftistHeap -> WeightLeftistHeap
deleteMin (T _ _ a b) = merge a b
