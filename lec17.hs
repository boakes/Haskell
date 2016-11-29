aList = [7,3,1,5,9]
bList = [2,4]
cList = append bList aList

append :: [a] -> [a] -> [a]
append [] lst = lst
append (x:xs) ys = x:(append xs ys)

sillyFunc (x:y:xs) = (x:(7:xs))

data Tree a = Node a (Tree a) (Tree a) | Leaf  deriving Show

treeA = Node 8 (Node 3 Leaf (singleton 7)) (Node 15 (singleton 9) (singleton 21))
singleton x = Node x Leaf Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = singleton x
insert x (Node y lft rgt) =
	case compare x y of
		EQ -> Node y lft rgt
		LT -> (Node y (insert x lft) rgt)
		GT -> (Node y lft (insert x rgt))

toTree :: Ord a => [a] -> Tree a
{-toTree [] = Leaf
toTree (x:xs) = (insert x (toTree xs))
-}
toTree xs = foldr insert Leaf xs