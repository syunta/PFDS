module Chapter5 where
import Data.SplayHeap

-- 5.1
-- (a)

-- Data.BatchedDeque

-- (b)

{-

cons:
(1) .. 空でないdeque で f < r の場合
実際のステップを1回行い、ポテンシャルを1減らすので償却コストは0

(2) .. 空でないdeque で r < f の場合
実際のステップを1回行い、ポテンシャルを1増やすので償却コストは2

(3) .. 先頭側を反転するdeque
length  -> n ステップ
splitAt -> n/2 ステップ
reverse -> n/2 ステップ
cons    -> 1 ステップ

実際のステップを 2n+1 回行い、ポテンシャルをn減らすので償却コストは 2n + 1 - n = n + 1 になってしまう？
deque に長さを保持すれば length しなくてよくなるので償却コストは 1 になるはず.


以下略

-}

-- 5.2

{-

貯金不変条件: 初期状態のヒープのt個の木、それぞれに貯金が1とする
insertの呼び出しは k + 1 ステップかかる(k=linkの回数)
insertの後では t - k + 1 の木が存在するので、t の貯金から k + 1 を消費する

(t - k + 1) - t = 1 - k の貯金が残り

最終的な償却コストは k + 1 + (1 - k) = 2

-}

-- 5.3

{-

merge:
(1) 入力の二項ヒープのいずれかが空の場合
実際のステップを1回行い、ポテンシャルを増やすことも減らすこともしないので 1

(2) 入力の二項ヒープがいずれも空でない場合
ポテンシャルは t, s の木の数とすると、初期状態のポテンシャルは t + s
mergeには k + logn くらいかかる(k=link,繰り上がりの回数)

  1 1 1 1
  0 0 1 0
1 0 0 0 1

mergeの後のポテンシャルは t + s から k (繰り上がりの回数)を引いたものなので、変化量は

(t + s - k) - (t + s) = -k

最終的な償却コストは k + logn -k = logn

-}

-- 5.4

bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
bigger _ E = E
bigger pivot (T a x b) =
  if x <= pivot then bigger pivot b
  else case a of
    E -> T E x b
    T a1 y a2 ->
      if y <= pivot then T (bigger pivot a2) x b
      else T (bigger pivot a1) y (T a2 x b)

smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
smaller _ E = E
smaller pivot (T a x b) =
  if pivot <= x then smaller pivot a
  else case b of
    E -> T a x E
    T a1 y a2 ->
      if pivot <= y then T a x (smaller pivot a1)
      else T (T a1 x a) y (smaller pivot a2)
