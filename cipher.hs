{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Data.Char
import Test.Hspec


shift :: Char -> Int -> Char
shift c n =
  let
    index = (ord c) - 65
    newIndex = mod (index + n) 26
  in
    chr (newIndex + 65)


shiftByLetter :: Char -> Char -> Char
shiftByLetter c1 c2 =
    let
      n = (ord c2) - 65
    in
      shift c1 n


zipLetters :: [Char] -> [Char] -> [(Char, Char)]
zipLetters [] _ = []
zipLetters _ [] = []
zipLetters text@(x:xs) keyword@(y:ys) =
  case x of
    ' ' -> (x, x) : zipLetters xs keyword
    _ -> (x, y) : zipLetters xs ys


encrypt :: String -> String -> String
encrypt text keyword =
  let
    pairs = zipLetters text (cycle keyword)
    convert c1 c2 =
      if c1 == ' ' then
        ' '
      else
        shiftByLetter c1 c2
  in
    [convert c1 c2 | (c1, c2) <- pairs]


main :: IO ()
main = hspec $ do
  describe "Vigenère cipher" $ do
    it "encrypt" $ do
      encrypt "MEET AT DAWN" "ALLY" `shouldBe` "MPPR AE OYWY"
