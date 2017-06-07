{- stack
  script
  --resolver lts-8.11
  --package QuickCheck
-}
import Test.QuickCheck


prop_powerAssociative :: Integer -> Integer -> Integer -> Bool
prop_powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_powerCommutative :: Integer -> Integer -> Bool
prop_powerCommutative x y =
  x ^ y == y ^ x


main :: IO ()
main = do
  -- quickCheck prop_powerAssociative
  quickCheck prop_powerCommutative
