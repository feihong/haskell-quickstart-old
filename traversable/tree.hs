{- stack
  script
  --resolver lts-8.11
  --package QuickCheck
  --package checkers
-}
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l v r) = (foldMap f l) `mappend` f v `mappend` (foldMap f r)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l v r) = Node <$> traverse f l <*> f v <*> traverse f r

-- e = Empty :: Tree (Sum Int)
-- l = Leaf (Sum 9)
-- t = Node Empty (Sum 1) (Leaf (Sum 2))

-- e = Empty :: Tree Int
-- l = Leaf 9
-- t = Node Empty 1 (Leaf 2)

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  x <- arbitrary
  l <- genTree
  r <- genTree
  frequency [ (1, return Empty)
            , (2, return $ Leaf x)
            , (2, return $ Node l x r) ]


main :: IO ()
main = do
  let trigger = undefined :: Tree (Int, Char, String)
  quickBatch (traversable trigger)
