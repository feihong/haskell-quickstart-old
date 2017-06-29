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


newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant $ x `mappend` y

instance Traversable (Constant a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Constant x) = fmap Constant (pure x)


instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return $ Constant x

instance Eq a => EqProp (Constant a b) where
  Constant x =-= Constant y = x `eq` y


main :: IO ()
main = do
  let trigger = undefined :: Constant Int (Char, String, [Int])
  quickBatch (traversable trigger)
