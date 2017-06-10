{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Data.List (elemIndex)
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Functor instance exercises" $ do
    it "pure" $ do
      pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) `shouldBe` Just 9

    it "two apply operators" $ do
      let y :: Maybe Integer
          y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
          z :: Maybe Integer
          z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

      pure (,) <*> y <*> z `shouldBe` Just (6,5)

    it "fmap and apply" $ do
      let x :: Maybe Int
          x = elemIndex 3 [1, 2, 3, 4, 5]

          y :: Maybe Int
          y = elemIndex 4 [1, 2, 3, 4, 5]

          max' :: Int -> Int -> Int
          max' = max

      max' <$> x <*> y `shouldBe` Just 3

    it "awkward sum" $ do
      let xs = [1, 2, 3]
          ys = [4, 5, 6]

          x :: Maybe Integer
          x = lookup 3 $ zip xs ys

          y :: Maybe Integer
          y = lookup 2 $ zip xs ys

      (,) <$> x <*> y `shouldBe` Just (6,5)
      (fmap sum $ (,) <$> x <*> y) `shouldBe` Just 5
