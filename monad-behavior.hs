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


j :: Monad m => m (m a) -> m a
j x = x >>= id


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f xs ys = do
  x <- xs
  y <- ys
  return $ f x y


l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f xs ys =
  xs >>=
  \x ->
    ys >>=
    \y ->
      return $ f x y


ap :: Monad m => m a -> m (a -> b) -> m b
ap xs fs = do
  x <- xs
  f <- fs
  return $ f x


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  h <- f x
  t <- meh xs f
  return $ h : t


flipType :: Monad m => [m a] -> m [a]
flipType xs =
    meh xs id


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

    it "j" $ do
      j [[1,2], [], [3]] `shouldBe` [1,2,3]
      j (Just (Just 1)) `shouldBe` Just 1
      j (Nothing :: Maybe (Maybe Int)) `shouldBe` Nothing

    it "l2" $ do
      l2 (,) [1,2] [4,5,6] `shouldBe` [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6)]
      l2' (,) [1,2] [4,5,6] `shouldBe` [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6)]

    it "ap" $ do
      ap [2,3,4] [(+1),(*2)] `shouldBe` [3,4,4,6,5,8]

    it "meh" $ do
      meh [1,2,3] (\x -> Just (x+1)) `shouldBe` Just [2,3,4]
      meh [1,2,3] (\x -> Nothing :: Maybe Int) `shouldBe` Nothing

    it "flipType" $ do
      flipType [Just 1,Just 2,Just 3] `shouldBe` Just [1,2,3]
      flipType [Just 1,Nothing,Just 3] `shouldBe` Nothing
