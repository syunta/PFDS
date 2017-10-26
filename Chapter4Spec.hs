import Test.Hspec
import Chapter4

main :: IO ()
main = hspec $ do
  describe "insertionSort" $ do
    it "sortElements" $ do
      insertionSort [5,3,2,7,1,4,6] `shouldBe` [1,2,3,4,5,6,7]
