import Data.List 
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

singleton x = Node x Leaf Leaf

myinsert :: Ord a => a -> Tree a -> Tree a
myinsert x Leaf = Node x Leaf Leaf
myinsert x (Node y lft rgt) =
    case compare x y of
        LT -> Node y (myinsert x lft) rgt
        GT -> Node y lft (myinsert x rgt)
        EQ -> Node y lft rgt

toTree :: Ord a => [a] -> Tree a
{-toTree [] = Leaf
toTree (x:xs) = insert x (toTree xs)-}
toTree xs = foldr myinsert Leaf xs

--1) 

greatest, least :: Tree a -> Maybe a
greatest Leaf = Nothing
greatest (Node x lft rgt) =
	case rgt of
	Leaf -> Just x
	_ -> greatest rgt

least Leaf = Nothing
least (Node x Leaf _ ) = Just x
least (Node _ lft _) = least lft	

{-
toBalTree :: Ord a => [a] -> Tree a
toBalTree [] = Leaf
toBalTree lst = helper(sort lst)
	where 
		helper [] = Leaf
		helper lst = 
			let 
				len = length lst 
				(smalls, bigs) = splitAt (len `div` 2) lst 
				label = head bigs
				lft = helper smalls
				rgt = helper (tail bigs)
	in Node label lft rgt


-}
structEq :: Eq a => Tree a -> Tree a -> Bool 
structEq Leaf Leaf = True
structEq (Node x xlft xrgt) (Node y ylft yrgt) = 
	(x == y) && (structEq xlft ylft) && (structEq xrgt yrgt)
structEq _ _ = False


toList :: Tree a -> [a]
toList Leaf = []
toList (Node x lft rgt) = (toList lft) ++ [x] ++ (toList rgt)

logEq :: Tree a -> Tree a -> Bool
logEq at bt = (toList at) == (toList bt)

{-
class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	x == y = not (x /= y)
	x /= y = not (x == y)
-}

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where 
		--(==) :: a -> a -> Bool
		(==) Red Red = True
		(==) Yellow Yellow = True
		(==) Green Green = True
		(==) _ _ = False

instance Eq (Tree a) where
	(==) ta tb = logEq ta tb

class Boolish a where
	toBool :: a -> Bool
	ifTruthy :: a -> b -> b -> b
	ifTruthy cond ifCase elseCase = if toBool cond then ifCase else elseCase

instance Boolish Integer where
	--toBool 4 = False
	--toBool 42 = True 
	toBool 0 = False
	toBool 4 = False
	toBool _ = True

instance Boolish [a] where
	toBool [] = False
	toBool _ = True

instance Boolish a => Boolish (Maybe a) where
	toBool Nothing = False
	toBool (Just a) = toBool a

instance Boolish TrafficLight where
	toBool Red = False
	toBool Green = True
	toBool Yellow = False

-- 2
{-
toBalTree :: Ord a => [a] -> Tree a
toBalTree [] = Leaf
toBalTree lst = 
	let slst = sort lst
	    len = length slst
	    (xs, ys) = splitAt len slst  
	    basdtree = Node myinsert (head ys) Leaf
	in foldr (foldr myinsert basdtree (reverse xs)) ys
-s\] -}


--3) 
{-
delete :: Ord a => a -> Tree a -> Tree a
4)
union, intersection :: Tree a -> Tree a -> Tree a
-}
