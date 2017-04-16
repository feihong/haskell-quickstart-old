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
      null [1] `shouldBe` False

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

    it "<$> (map operator)" $ do
      (+ 1) <$> [1..10] `shouldBe` [2..11]

    it "filter" $ do
      filter (\n -> n `mod` 3 == 0) [1..20] `shouldBe` [3, 6, 9, 12, 15, 18]

    it "concat" $ do
      concat [[1..3], [5..7]] `shouldBe` [1, 2, 3, 5, 6, 7]

    it "elem" $ do
      elem 4 [1..10] `shouldBe` True
      4 `elem` [1..10] `shouldBe` True
      -1 `elem` [1..10] `shouldBe` False

    it "range with step" $ do
      [1, 4..20] `shouldBe` [1, 4, 7, 10, 13, 16, 19]

    it "backwards range with step" $ do
      [20, 16..1] `shouldBe` [20, 16, 12, 8, 4]

    it "drop & take with infinite range" $ do
      (take 6 $ drop 10000 $ [1..]) `shouldBe` [10001..10006]

    it "take with repeat" $ do
      (take 3 $ repeat 5) `shouldBe` [5, 5, 5]

    it "replicate" $ do
      replicate 3 5 `shouldBe` [5, 5, 5]

    it "zip" $ do
      zip [1..10] ['a'..'f'] `shouldBe`
        [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f')]

    it "list comprehension" $ do
      [ x - 1 | x <- [5..66], x /= 45, x `mod` 15 == 0 ] `shouldBe` [14, 29, 59]
