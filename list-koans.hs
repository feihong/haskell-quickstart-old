{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "List koans" $ do
    it "head" $ do
      head [1, 2, 3] `shouldBe` (1 :: Int)

    it "tail" $ do
      tail [1, 2, 3] `shouldBe` [2, 3]

    it "map" $ do
      map (\n -> n + 1) [1, 2, 3] `shouldBe` [2, 3, 4]

    it "<$>" $ do
      (+ 1) <$> [1..10] `shouldBe` [2..11]
