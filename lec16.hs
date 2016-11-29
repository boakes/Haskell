
{-
data Tree a = Binary a (Tree a) (Tree a) | Unary a (Tree a) | Leaf a
--Node 8 (Unary 3 (Leaf 7)) (Binary 15 (Leaf 9) (Leaf 21)) 
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) | Leaf a
--Node 8 (Node 3 Nothing (Just (Leaf 7))) (Just (Node 15 (Just (Leaf 9) (Just (Leaf 21))) 
-}

data Tree a = Node a (Tree a) (Tree a) | Leaf  deriving Show
--Node 8 (Node 3 Empty (Leaf 7)) (Node 15 (Leaf 9) (Leaf 21)) 

treeA = Node 8 (Node 3 Leaf (singleton 7)) (Node 15 (singleton 9) (singleton 21))
singleton x = Node x Leaf Leaf

element :: Ord a => a -> Tree a -> Bool
element x Leaf = False
element x (Node y lft rgt) = 
	case compare x y of
		EQ -> True
		LT -> element x lft
		GT -> element x rgt

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = singleton x
insert x (Node y lft rgt) =
	case compare x y of
		EQ -> Node y lft rgt
		LT -> (Node y (insert x lft) rgt)
		GT -> (Node y lft (insert x rgt))

