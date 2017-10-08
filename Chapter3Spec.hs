import Test.Hspec
import Data.Heap
import Data.LeftistHeap
import qualified Data.WeightLeftistHeap as W
import qualified Data.BinomialHeap as B
import qualified Data.BinomialHeap' as B'
import qualified Data.ExplicitMinHeap as E
import Chapter3

(-:) :: a -> (a -> b) -> b
x -: f = f x

main :: IO ()
main = hspec $ do
  describe "insert'" $ do
    it "inserts element to the leftist heap" $ do
      (insert 5 E -: insert 8 -: insert 4 -: insert 1 -: insert 10) `shouldBe`
        (insert' 5 E -: insert' 8 -: insert' 4 -: insert' 1 -: insert' 10)
  describe "fromList" $ do
    it "generates a LeftistHeap from array" $ do
      merge (insert 7 E -: insert 2) (insert 1 E -: insert 4) `shouldBe` fromList [7,2,1,4]
  describe "merge'" $ do
    it "merges WeightLeftistHeap" $ do
      merge' (W.insert 7 W.E -: W.insert 2) (W.insert 1 W.E -: W.insert 4) `shouldBe`
        W.merge (W.insert 7 W.E -: W.insert 2) (W.insert 1 W.E -: W.insert 4)
  describe "findMin'" $ do
    it "finds smallest root from BinomialHeap" $ do
      findMin' (B.insert 1 B.empty -: B.insert 5 -: B.insert 3) `shouldBe`
        B.findMin (B.insert 1 (B.BH []) -: B.insert 5 -: B.insert 3)
  describe "BinomialHeap'" $ do
    it "behaves like BinomialHeap" $ do
      let t' = B'.insert 1 B'.empty -: B'.insert 5 -: B'.insert 3
          t  = B.insert 1 B.empty -: B.insert 5 -: B.insert 3
      B'.findMin t' `shouldBe` B.findMin t
      map B'.rank (B'.tree $ B'.deleteMin t') `shouldBe` map B.rank (B.tree $ B.deleteMin t)
      map B'.root (B'.tree $ B'.deleteMin t') `shouldBe` map B.root (B.tree $ B.deleteMin t)
  describe "ExplicitMinHeap" $ do
    it "behaves like BinomialHeap" $ do
      let b = B.insert 2 (B.BH []) -: B.insert 5 -: B.insert 3
          l = insert 2 E -: insert 5 -: insert 3
          eb = E.insert 1 (E.NE 2 b) :: E.ExplicitMinHeap B.BinomialHeap Int
          el = E.insert 1 (E.NE 2 l) :: E.ExplicitMinHeap LeftistHeap Int
      E.findMin eb `shouldBe` E.findMin el
