{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)


tests = hspec $ do
  describe "Traversable instance for Identity" $ do
    it "with Maybe" $ do
      traverse Just (Identity 4) `shouldBe` Just (Identity 4)

    it "with tuple" $ do
      traverse ((,) "what") (Identity 4) `shouldBe` ("what",Identity 4)

    it "with list" $ do
      traverse (\x -> [x]) (Identity 4) `shouldBe` [Identity 4]

main :: IO ()
main = do
  tests
