import Test.Hspec
import Data.LeftistHeap
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
