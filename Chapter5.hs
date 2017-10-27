module Chapter5 where
import qualified Data.BatchedDeque as D

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
