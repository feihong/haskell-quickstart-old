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


unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)


treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold (\a ->
  case a of
    0 -> Nothing
    x -> Just (x-1, x-1, x-1))


tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


main :: IO ()
main = hspec $ do
  describe "Binary tree exercises" $ do
    it "inorder" $ do
      inorder tree `shouldBe` [1,2,3]

    it "foldTree" $ do
      foldTree ((:) . (*2)) [] tree `shouldBe` [2,4,6]

    it "unfold" $ do
      let
        f 0 = Just (1, 0, 1)
        f 1 = Just (2, 1, 2)
        f _ = Nothing

      unfold f 2 `shouldBe` Leaf
      unfold f 1 `shouldBe` Node Leaf 1 Leaf
      unfold f 0 `shouldBe` Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)

    it "treeBuild" $ do
      treeBuild 0 `shouldBe` Leaf
      treeBuild 1 `shouldBe` Node Leaf 0 Leaf
      treeBuild 2 `shouldBe` Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)
      treeBuild 3 `shouldBe` Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf)) 2 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 0 Leaf))
