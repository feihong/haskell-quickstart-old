{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x]

-- Write without using do syntax
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>=
  \x -> if even x
    then [x*x, x*x]
    else [x]


main :: IO ()
main = hspec $ do
  describe "Monads" $ do
    it "twiceWhenEven" $ do
      twiceWhenEven [1..8] `shouldBe` [1,4,4,3,16,16,5,36,36,7,64,64]
      twiceWhenEven' [1..8] `shouldBe` [1,4,4,3,16,16,5,36,36,7,64,64]
