module Data.LeftistHeap where

data LeftistHeap = E | T Int Int LeftistHeap LeftistHeap deriving (Show,Eq)

merge :: LeftistHeap -> LeftistHeap -> LeftistHeap
merge E h = h
merge h E = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

rank :: LeftistHeap -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: Int -> LeftistHeap -> LeftistHeap -> LeftistHeap
makeT x a b
  | rank a >= rank b = T (rank b+1) x a b
  | otherwise        = T (rank a+1) x b a

insert :: Int -> LeftistHeap -> LeftistHeap
insert x h = merge (T 1 x E E) h

findMin :: LeftistHeap -> Int
findMin (T _ x _ _) = x

deleteMin :: LeftistHeap -> LeftistHeap
deleteMin (T _ x a b) = merge a b
