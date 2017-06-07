{- stack
  script
  --resolver lts-8.11
  --package hspec
  --package QuickCheck
-}
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck


half x = x / 2
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


main :: IO ()
main = hspec $ do
  describe "Property tests" $ do
    it "halfIdentity is equivalent to identity" $ do
      property $ \x -> halfIdentity x == (x :: Double)

    it "sorted list is ordered" $ do
      property $ \x -> listOrdered $ sort (x :: [Integer])
