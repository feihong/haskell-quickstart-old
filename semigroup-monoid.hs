{- stack
  script
  --resolver lts-8.11
  --package QuickCheck
-}
import Data.Semigroup
import Test.QuickCheck


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  mappend mempty a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  mappend a mempty == a


data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a1 b1) (Two a2 b2) =
    Two (mappend a1 a2) (mappend b1 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoIntString = Two (Product Int) String
type TwoAssoc = TwoIntString-> TwoIntString -> TwoIntString -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoIntString -> Bool)
  quickCheck (monoidRightIdentity :: TwoIntString -> Bool)
