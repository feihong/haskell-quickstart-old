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

prop_foldrConsAndConcat :: [Int] -> [Int] -> Bool
prop_foldrConsAndConcat x y =
  foldr (:) x y == x ++ y


main :: IO ()
main = do
  -- quickCheck prop_powerAssociative
  -- quickCheck prop_powerCommutative
  quickCheck prop_foldrConsAndConcat
