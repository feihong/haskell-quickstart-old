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


data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)


data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa  (fmap h fa) (fmap h ga)


main :: IO ()
main = hspec $ do
  describe "Functor instance exercises" $ do
    it "Quant" $ do
      fmap (+1) Finance `shouldBe` (Finance :: QuantCI)
      fmap (+1) (Desk 'c') `shouldBe` (Desk 'c' :: QuantCI)
      fmap (+1) (Bloor 55) `shouldBe` (Bloor 56 :: QuantCI)

    it "LiftItOut" $ do
      fmap (*2) (LiftItOut [4]) `shouldBe` LiftItOut [8]

    it "Parappa" $ do
      fmap (+1) (DaWrappa [44] (Just 55)) `shouldBe` DaWrappa [45] (Just 56)
      fmap (+1) (DaWrappa [] Nothing) `shouldBe` DaWrappa [] Nothing
