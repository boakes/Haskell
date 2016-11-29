--import Control.Applicative

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

thirdElemPretty lst = do
    xs <- safeTail lst
    ys <- safeTail xs
    x  <- safeHead ys
    Just (x+1)

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    --pure :: a -> Maybe a
    pure x = Just x
    --(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    --(Just f) <*> (Just x) = Just $ f x
    --_ <*> _ = Nothing

    (Just f) <*> mx = fmap f mx
    Nothing <*> mx = Nothing
    
    

instance Applicative [] where
    --pure :: a -> [a]
    pure x = [x]
    -- <*> :: [a -> b] -> [a] -> [b]
    [] <*> xs = []
    (f:fs) <*> xs = (map f xs) ++  (fs <*> xs)


