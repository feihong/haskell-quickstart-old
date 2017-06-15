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

type ESI = Either String Integer


main :: IO ()
main = hspec $ do
  describe "Monads" $ do
    it "twiceWhenEven" $ do
      twiceWhenEven [1..8] `shouldBe` [1,4,4,3,16,16,5,36,36,7,64,64]
      twiceWhenEven' [1..8] `shouldBe` [1,4,4,3,16,16,5,36,36,7,64,64]

    it "Maybe" $ do
      let nothing :: Maybe Integer
          nothing = Nothing

      (Just 4 >>= (\x -> Just 6)) `shouldBe` Just 6
      (Just 4 >>= (\x -> Nothing)) `shouldBe` nothing
      (Nothing >>= (\x -> Nothing)) `shouldBe` nothing
      (Nothing >>= (\x -> Just 6)) `shouldBe` nothing

    it "Either" $ do
      let input :: Either String Integer
          input = Right 8
          err :: Either String Double
          err = Left "err"

      (input >>= (\x -> Right 3)) `shouldBe` Right 3
      (input >>= (\x -> err)) `shouldBe` Left "err"
      (err >>= (\x -> err)) `shouldBe` Left "err"
      (err >>= (\x -> Right 4)) `shouldBe` Left "err"
