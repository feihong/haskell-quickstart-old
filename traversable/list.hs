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


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable List where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ Nil = pure Nil
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
    h <- arbitrary
    t <- genList
    frequency [ (3, return $ Cons h t)
              , (1, return Nil) ]

instance Eq a => EqProp (List a) where
    (=-=) = eq


main :: IO ()
main = do
  let trigger = undefined :: List (Int, Char, String)
  quickBatch (traversable trigger)
