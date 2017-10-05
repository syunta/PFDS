import Test.Hspec
import Data.LeftistHeap
import qualified Data.WeightLeftistHeap as W
import qualified Data.BinomialHeap as B
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
      findMin' (B.insert 1 [] -: B.insert 5 -: B.insert 3) `shouldBe`
        B.findMin (B.insert 1 [] -: B.insert 5 -: B.insert 3)
