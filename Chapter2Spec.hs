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
