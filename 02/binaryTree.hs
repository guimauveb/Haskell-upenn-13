data BinaryTree a = 
  Empty
  | Node (BinaryTree a) a (BinaryTree a)
  | Leaf a
  deriving (Show)

ls = Node (Node (Leaf 5) 3 (Leaf 4)) 1 (Leaf 2)
testTree = Node (Node (Leaf 5) 3 (Leaf 4)) 1 (Leaf 2)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node leftSubTree a rightSubTree) = Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)
 
main = print("")

