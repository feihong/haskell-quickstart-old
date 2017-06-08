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


-- Product monoid
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


-- Function monoid
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend = (<>)

-- Another function monoid
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f <> g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp (\_ -> mempty)
  mappend = (<>)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoIntString -> Bool)
  quickCheck (monoidRightIdentity :: TwoIntString -> Bool)
