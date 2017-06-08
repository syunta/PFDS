import Test.Hspec
import Data.FiniteMap
import Chapter2
import Prelude hiding (lookup)

main :: IO ()
main = hspec $ do
  describe "member" $ do
    it "returns whether the tree contains element" $ do
      let t = T (T (T E 12 E) 14 (T E 16 E)) 18 (T (T E 20 E) 22 (T E 24 E))
      member 22 t `shouldBe` True
      member 14 t `shouldBe` True
      member 18 t `shouldBe` True
      member 12 t `shouldBe` True
      member 19 t `shouldBe` False
  describe "insert" $ do
    it "inserts element to the tree" $ do
      let t1 = T E 18 E
      insert 18 t1 `shouldBe` T E 18 E
      insert 22 t1 `shouldBe` T E 18 (T E 22 E)
      insert 14 t1 `shouldBe` T (T E 14 E) 18 E
      let t2 = insert 22 (insert 14 (insert 18 E))
      insert 12 t2 `shouldBe` T (T (T E 12 E) 14 E) 18 (T E 22 E)
  describe "insert'" $ do
    it "inserts element to the tree" $ do
      let t1 = T E 18 E
      insert' 18 t1 `shouldBe` T E 18 E
      insert' 22 t1 `shouldBe` T E 18 (T E 22 E)
      insert' 14 t1 `shouldBe` T (T E 14 E) 18 E
      let t2 = insert' 22 (insert' 14 (insert' 18 E))
      insert' 12 t2 `shouldBe` T (T (T E 12 E) 14 E) 18 (T E 22 E)
  describe "complete" $ do
    it "makes a tree whose depth is n and contains same element" $ do
      complete 1 3 `shouldBe` T (T (T E 1 E) 1 (T E 1 E)) 1 (T (T E 1 E) 1 (T E 1 E))
  describe "create" $ do
    it "makes an unbalanced tree whose node size is m and contains same element" $ do
      create 1 1 `shouldBe` T E 1 E
      create 1 2 `shouldBe` T E 1 (T E 1 E)
      create 1 3 `shouldBe` T (T E 1 E) 1 (T E 1 E)
      create 1 4 `shouldBe` T (T E 1 E) 1 (T E 1 (T E 1 E))
  describe "UnbalancedMap" $ do
    it "provides data structure binary tree map" $ do
      let m = bind 22 7 (bind 14 2 (bind 18 5 Empty))
      bind 12 3 m `shouldBe` M (M (M Empty (12,3) Empty) (14,2) Empty) (18,5) (M Empty (22,7) Empty)
      lookup 14 m `shouldBe` Just 2
