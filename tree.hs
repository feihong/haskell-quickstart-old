{- stack
  script
  --resolver lts-8.11
  --package hspec
-}
import Test.Hspec


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z tree = foldr f z $ inorder tree


tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


main :: IO ()
main = hspec $ do
  describe "Binary tree exercises" $ do
    it "inorder" $ do
      inorder tree `shouldBe` [1,2,3]

    it "foldTree" $ do
      foldTree ((:) . (*2)) [] tree `shouldBe` [2,4,6]
