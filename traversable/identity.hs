{- stack
  script
  --resolver lts-8.11
  --package hspec
  --package QuickCheck
  --package checkers
-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)


instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = fmap Identity (f x)
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA (Identity x) = fmap Identity x


tests = hspec $ do
  describe "Traversable instance for Identity" $ do
    it "with Maybe" $ do
      traverse Just (Identity 4) `shouldBe` Just (Identity 4)

    it "with tuple" $ do
      traverse ((,) "what") (Identity 4) `shouldBe` ("what",Identity 4)

    it "with list" $ do
      traverse (\x -> [x]) (Identity 4) `shouldBe` [Identity 4]


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


main :: IO ()
main = do
  tests

  let trigger = undefined :: Identity (String, Char, [Int])
  quickBatch (traversable trigger)
