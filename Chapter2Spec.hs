import Test.Hspec
import Chapter2

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
    it "inserts element to tree" $ do
      let t1 = T E 18 E
      insert 18 t1 `shouldBe` T E 18 E
      insert 22 t1 `shouldBe` T E 18 (T E 22 E)
      insert 14 t1 `shouldBe` T (T E 14 E) 18 E
      let t2 = insert 22 (insert 14 (insert 18 E))
      insert 12 t2 `shouldBe` T (T (T E 12 E) 14 E) 18 (T E 22 E)
