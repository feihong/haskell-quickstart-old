{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec


myOr :: [Bool] -> Bool
myOr = foldr (||) True


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a then True else b) False


myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)


myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []


main :: IO ()
main = hspec $ do
  describe "Fold exercises" $ do
    it "myOr" $ do
      myOr [True, False, True] `shouldBe` True
      myOr (replicate 15 False) `shouldBe` True

    it "myAny" $ do
      myAny even [1, 2, 3, 4] `shouldBe` True
      myAny odd [2, 4, 6] `shouldBe` False
      myAny even [] `shouldBe` False

    it "myElem" $ do
      myElem 1 [1..4] `shouldBe` True
      myElem 1 [2..4] `shouldBe` False

    it "myReverse" $ do
      myReverse [1..4] `shouldBe` [4,3..1]
      myReverse ([] :: [Int]) `shouldBe` []
