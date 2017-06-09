#!/usr/bin/env stack
{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec


data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor $ f x

type QuantCI = Quant Char Int


main :: IO ()
main = hspec $ do
  describe "Functor instance exercises" $ do
    it "Quant" $ do
      fmap (+1) Finance `shouldBe` (Finance :: QuantCI)
      fmap (+1) (Desk 'c') `shouldBe` (Desk 'c' :: QuantCI)
      fmap (+1) (Bloor 55) `shouldBe` (Bloor 56 :: QuantCI)
