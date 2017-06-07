{- stack
  script
  --resolver lts-8.11
  --package hspec
  --package QuickCheck
-}
import Data.List (sort)
-- import Test.Hspec
import Test.QuickCheck


half x = x / 2
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x =
  halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_listOrdered x =
    listOrdered $ sort (x :: [Integer])

prop_plusAssociative x y z =
  (x :: Integer) + (y + z) == (x + y) + z


main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
