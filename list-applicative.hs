{- stack
  script
  --resolver lts-8.11
  --package QuickCheck
  --package checkers
-}
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

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  -- (Cons f t1) <*> l2 = append (fmap f l2) (t1 <*> l2)
  l1 <*> l2 = flatMap (\f -> fmap f l2) l1

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

-- Cons (+1) (Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil)
-- ~ Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- TESTING

instance Arbitrary a => Arbitrary (List a) where
  -- Is there a better way to do this?
  arbitrary =
    frequency [
      (10, oneElemList),
      (10, twoElemList),
      (1, return Nil)
    ]
    where
      oneElemList = do
        x <- arbitrary
        return $ Cons x Nil
      twoElemList = do
        x <- arbitrary
        y <- arbitrary
        return $ Cons x (Cons y Nil)

instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  let trigger = undefined :: List (String, Int, Char)
  quickBatch $ applicative trigger
