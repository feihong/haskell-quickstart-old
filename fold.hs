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


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\b a ->
  if f b a == GT then b else a) x (x:xs)


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\b a ->
  if f b a == LT then b else a) x (x:xs)



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

    it "myFilter" $ do
      myFilter even [1..7] `shouldBe` [2,4,6]
      myFilter odd [2,4..100] `shouldBe` []

    it "squish" $ do
      squish [[1,2,3], [4,5,6], [7,8,9,10]] `shouldBe` [1..10]

    it "squishMap" $ do
      squishMap (replicate 2) ['a', 'b', 'c'] `shouldBe` "aabbcc"

    it "squishAgain" $ do
      squishAgain [[1,2,3], [4,5,6], [7,8,9,10]] `shouldBe` [1..10]

    it "myMaximumBy" $ do
      myMaximumBy compare [1,5,8,4,3,7] `shouldBe` 8
      myMaximumBy (\_ _ -> GT) [1..10] `shouldBe` 1
      myMaximumBy (\_ _ -> LT) [1..10] `shouldBe` 10

    it "myMinimumBy" $ do
      myMinimumBy compare [1,5,8,2,-3,4,3,7] `shouldBe` -3
      myMinimumBy (\_ _ -> GT) [1..10] `shouldBe` 10
      myMinimumBy (\_ _ -> LT) [1..10] `shouldBe` 1
