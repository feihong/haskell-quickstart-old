{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Control.Applicative (liftA2)
import Test.Hspec


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

tupled :: [Char] -> ([Char], [Char])
tupled =
  (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  x <- cap
  y <- rev
  return (x,y)

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader  r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra =
    Reader $ \r -> (rab r) (ra r)


ask :: Reader a a
ask = Reader (\x -> x)    -- id

asks2 :: (r -> a) -> Reader r a
asks2 f = Reader f

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m


main :: IO ()
main = hspec $ do
  describe "Reader exercises" $ do
    it "applicative for functions" $ do
      ((+1) <$> (*2)) 3 `shouldBe` 7
      ((+) <$> (*3) <*> (^2)) 4 `shouldBe` 28

    it "tupled" $ do
      tupled "wonkee" `shouldBe` ("WONKEE","eeknow")
      tupled' "wonkee" `shouldBe` ("WONKEE","eeknow")

    it "bolt" $ do
      bolt 3 `shouldBe` False
      bolt 8 `shouldBe` False
      bolt 5 `shouldBe` True
      bolt 4 `shouldBe` True

    it "sequA" $ do
      sequA 0 `shouldBe` [False,True,True]
      sequA 4 `shouldBe` [True,True,True]
      sequA 5 `shouldBe` [True,True,False]
