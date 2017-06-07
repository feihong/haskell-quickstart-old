{- stack
  script
  --resolver lts-8.11
  --package QuickCheck
-}
import Data.List (sort)
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

prop_plusCommutative x y =
  (x :: Float) + y == y + x

prop_quotAndRem x y =
  let
    y' = if y == 0 then -1 else y
  in
    (quot x y') * y' + (rem x y') == (x :: Integer)

prop_divAndMod x y =
  let
    y' = if y == 0 then -1 else y
  in
    (div x y') * y' + (mod x y') == (x :: Integer)

prop_foldrConcatEmptyAndConcatFunc :: [[Int]] -> Bool
prop_foldrConcatEmptyAndConcatFunc xs =
  foldr (++) [] xs == concat xs


main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_quotAndRem
  quickCheck prop_divAndMod
  quickCheck prop_foldrConcatEmptyAndConcatFunc
