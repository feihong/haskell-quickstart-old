{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Data.Char
import Test.Hspec


isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l@(x:xs) (y:ys) =
  if x == y then
    isSubsequenceOf xs ys
  else
    isSubsequenceOf l ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords s =
  [(w, toUpper x : xs) | w@(x:xs) <- words s]


main :: IO ()
main = hspec $ do
  describe "As-patterns" $ do
    it "isSubsequenceOf" $ do
      isSubsequenceOf "blah" "blah" `shouldBe` True
      isSubsequenceOf "burger" "burgerboy" `shouldBe` True
      isSubsequenceOf "burger" "pooburger" `shouldBe` True
      isSubsequenceOf "burger" "pooburgeryikes" `shouldBe` True
      isSubsequenceOf "burger" "poob1u2r3g4eryikes" `shouldBe` True

    it "capitalizeWords" $ do
      capitalizeWords "hello world" `shouldBe`
        [("hello", "Hello"), ("world", "World")]
