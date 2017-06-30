{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
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


ask :: Reader a a
ask = Reader (\x -> x)    -- id


main :: IO ()
main = hspec $ do
  describe "Reader exercises" $ do
    it "applicative for functions" $ do
      ((+1) <$> (*2)) 3 `shouldBe` 7
      ((+) <$> (*3) <*> (^2)) 4 `shouldBe` 28

    it "tupled" $ do
      tupled "wonkee" `shouldBe` ("WONKEE","eeknow")
      tupled' "wonkee" `shouldBe` ("WONKEE","eeknow")
