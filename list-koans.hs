{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "List koans" $ do
    it "null" $ do
      null [] `shouldBe` True

    it "init" $ do
      init [1..6] `shouldBe` [1, 2, 3, 4, 5]

    it "last" $ do
      last [1..6] `shouldBe` 6

    it "head" $ do
      head [1, 2, 3] `shouldBe` (1 :: Int)

    it "head on empty list" $ do
      head [] `shouldThrow` anyErrorCall

    it "tail" $ do
      tail [1, 2, 3] `shouldBe` [2, 3]

    -- it "tail on empty list" $ do
    --   tail [] `shouldThrow` anyErrorCall

    it "tail on single-element list" $ do
      tail [1] `shouldBe` []
      tail [1] `shouldSatisfy` null

    it "map" $ do
      map (\n -> n + 1) [1, 2, 3] `shouldBe` [2, 3, 4]

    it "<$>" $ do
      (+ 1) <$> [1..10] `shouldBe` [2..11]

    it "filter" $ do
      filter (\n -> n `mod` 3 == 0) [1..20] `shouldBe` [3, 6, 9, 12, 15, 18]
