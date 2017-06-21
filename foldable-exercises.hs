{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Data.Monoid
import Test.Hspec


data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two x y) = f y z
  foldMap f (Two x y) = f y


-- Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)


main :: IO ()
main = hspec $ do
  describe "Foldable exercises" $ do
    it "sum" $ do
      let sum :: (Foldable t, Num a) => t a -> a
          sum = getSum . foldMap Sum

      sum [1,2,3,4] `shouldBe` 10
      sum [] `shouldBe` 0

    it "product" $ do
      let product :: (Foldable t, Num a) => t a -> a
          product = getProduct . foldMap Product

      product [1,2,3,4] `shouldBe` 24
      product [] `shouldBe` 1

    it "elem" $ do
      let elem :: (Foldable t, Eq a) => a -> t a -> Bool
          elem x = foldr (\a b -> if a == x then True else b) False

      elem 3 [1,2,3,4] `shouldBe` True
      elem 5 [1,2,3,4] `shouldBe` False
      elem 1 [] `shouldBe` False

    it "minimum" $ do
      let minimum :: (Foldable t, Ord a) => t a -> Maybe a
          minimum = foldr (\a b ->
            case b of
              Nothing -> Just a
              Just b' -> if b' < a then Just b' else Just a) Nothing

      minimum [1,2,0,4] `shouldBe` Just 0
      minimum ([] :: [Maybe Int]) `shouldBe` Nothing

    it "maximum" $ do
      let maximum :: (Foldable t, Ord a) => t a -> Maybe a
          maximum = foldr (\a b ->
            case b of
              Nothing -> Just a
              Just b' -> if b' > a then Just b' else Just a) Nothing

      maximum [1,22,0,4] `shouldBe` Just 22
      maximum ([] :: [Maybe Int]) `shouldBe` Nothing

    it "null" $ do
      let null :: (Foldable t) => t a -> Bool
          null = foldr (\_ _ -> False) True

      null [] `shouldBe` True
      null [1] `shouldBe` False
      null Nothing `shouldBe` True
      null (Just 1) `shouldBe` False

    it "length" $ do
      let length :: (Foldable t) => t a -> Int
          length = foldr (\_ b -> 1+b) 0

      length [] `shouldBe` 0
      length [1..10] `shouldBe` 10
      length Nothing `shouldBe` 0
      length (Just 6) `shouldBe` 1

    it "toList" $ do
      let toList :: (Foldable t) => t a -> [a]
          toList = foldr (\a b -> a:b) []

      toList (Nothing :: Maybe Int) `shouldBe` []
      toList (Just 7) `shouldBe` [7]

    it "fold" $ do
      let fold :: (Foldable t, Monoid m) => t m -> m
          fold = foldMap id

      fold [Sum 1, Sum 2, Sum 3] `shouldBe` Sum 6

    it "foldMap" $ do
      let foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
          foldMap f = foldr (\a b -> f a `mappend` b) mempty

      foldMap Product [2,3,4] `shouldBe` Product 24
      foldMap show [2,3,4] `shouldBe` "234"

    it "filterF" $ do
      filterF even [1,2,3,4] `shouldBe` [2,4]
      filterF (const True) [1..7] `shouldBe` [1..7]
      filterF (\x -> x == 66) [1,2,3,66,4,5,6] `shouldBe` [66]
