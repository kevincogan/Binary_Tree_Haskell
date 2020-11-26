data Tree t = Empty | Root t (Tree t) (Tree t)
						deriving (Eq, Ord, Show)


insert :: Ord a => a -> Tree a -> Tree a
insert new_node Empty = Root new_node Empty Empty
insert new_node (Root parent_node left_child right_child)
	| new_node == parent_node = (Root parent_node (insert new_node left_child) right_child)
	| new_node < parent_node = (Root parent_node (insert new_node left_child) right_child)
	| new_node > parent_node = (Root parent_node left_child (insert new_node right_child))


search :: Ord a => a -> Tree a -> Bool
search search_node Empty = False
search search_node (Root parent_node left_child right_child)
  | search_node == parent_node = True
  | search_node < parent_node  = search search_node left_child
  | search_node > parent_node  = search search_node right_child


preorder :: Ord a => Tree a -> [a]
preorder Empty = []
preorder (Root parent_node left_child right_child) = parent_node : preorder left_child ++ preorder right_child


inorder :: Ord a => Tree a -> [a]
inorder Empty = []
inorder (Root parent_node left_child right_child) = inorder left_child ++ [parent_node] ++ inorder right_child

postorder :: Ord a => Tree a -> [a]
postorder Empty = []
postorder (Root parent_node left_child right_child) = postorder left_child ++ postorder right_child ++ parent_node : []
