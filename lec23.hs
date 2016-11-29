data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)
singleton x = Node x Leaf Leaf
treeA = Node 8 (Node 3 Leaf (singleton 7)) (Node 15 (singleton 9) (singleton 21))

mapT :: (a -> b) -> Tree a -> Tree b
mapT f Leaf = Leaf
mapT f (Node x lft rgt) = Node (f x) (mapT f lft) (mapT f rgt)

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}
instance Functor Tree where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f tree = mapT f tree

{-
instance Functor [] where
    fmap = map
    -}

class Boolish a where
    toBool :: a -> Bool
    ifTruthy :: a -> b -> b -> b
    ifTruthy cond ifCase elseCase = if toBool cond then ifCase else elseCase

--data Maybe a = Just a | Nothing

instance Boolish (Maybe a) where
    --toBool :: (Maybe a) -> Bool
    toBool (Just x) = True
    toBool Nothing  = False

{-
instance Functor Maybe where
    --fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f (Just x) = Just $ f x
    fmap f Nothing  = Nothing
    -}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

thirdElement :: Num a => [a] -> Maybe a
thirdElement lst = 
    case safeTail lst of
        Nothing -> Nothing
        (Just xs) -> 
            case safeTail xs of
                Nothing -> Nothing
                Just ys -> 
                    case safeHead ys of
                        Nothing -> Nothing
                        Just x -> Just (x + 1)

{-
thirdElement' :: Num a => [a] -> Maybe a
thirdElement' lst =
    let mxs = safeTail lst
        mys = fmap safeTail mxs
        mx = fmap safeHead mys
        mxp = fmap (+1) mx
    in mxp
-}
