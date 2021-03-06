import Test.Hspec
import Chapter5
import qualified Data.BatchedDeque as D
import qualified Data.SplayHeap as S

main :: IO ()
main = hspec $ do
  describe "BatchedDeque" $ do
    describe "cons" $ do
      it "takes an element and adds front deque" $ do
        D.cons (D.Q [2,3] [5,4]) 1 `shouldBe` D.Q [1,2,3] [5,4]
        D.cons (D.Q [2,3,4,5] []) 1 `shouldBe` D.Q [1,2,3] [5,4]
        D.cons (D.Q [] [5,4,3,2]) 1 `shouldBe` D.Q [1,2,3] [5,4]
    describe "head" $ do
      it "returns an element from front deque" $ do
        D.head (D.Q [1,2] [4,3]) `shouldBe` 1
    describe "tail" $ do
      it "returns deque except front element" $ do
        D.tail (D.Q [1,2] [4,3]) `shouldBe` D.Q [2] [4,3]
        D.tail (D.Q [] [4,3,2,1]) `shouldBe` D.Q [2] [4,3]
        D.tail (D.Q [1,2,3,4] []) `shouldBe` D.Q [2] [4,3]
    describe "snoc" $ do
      it "takes an element and adds rear deque" $ do
        D.snoc (D.Q [1,2] [4,3]) 5 `shouldBe` D.Q [1,2] [5,4,3]
        D.snoc (D.Q [1,2,3,4] []) 5 `shouldBe` D.Q [1,2] [5,4,3]
        D.snoc (D.Q [] [4,3,2,1]) 5 `shouldBe` D.Q [1,2] [5,4,3]
    describe "last" $ do
      it "returns an element from rear deque" $ do
        D.last (D.Q [1,2] [4,3]) `shouldBe` 4
    describe "init" $ do
      it "returns deque except rear element" $ do
        D.init (D.Q [1,2] [4,3]) `shouldBe` D.Q [1,2] [3]
        D.init (D.Q [] [4,3,2,1]) `shouldBe` D.Q [1,2] [3]
        D.init (D.Q [1,2,3,4] []) `shouldBe` D.Q [1,2] [3]
  describe "SplayHeap" $ do
    describe "smaller" $ do
      it "returns balanced SplayHeap contains elements smaller than pivot" $ do
        smaller 10 (S.T S.E 1 (S.T S.E 2 (S.T S.E 3 (S.T S.E 4 S.E)))) `shouldBe`
          S.T (S.T S.E 1 S.E) 2 (S.T (S.T S.E 3 S.E) 4 S.E)
        smaller 10 (S.T S.E 10 S.E) `shouldBe` S.E
