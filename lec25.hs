
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

thirdElementBetter lst = 
    let mxs = safeTail lst
        mys = applyMaybe safeTail mxs
        mx  = applyMaybe safeHead mys
    in --applyMaybe (\x -> Just (x + 1)) mx
       mx >>= (\x -> Just (x + 1)) 

thirdElementBestest lst =
    (return lst) >>= safeTail >>= safeTail >>= safeHead >>= (\x -> return $ x + 1)

-- fmap :: (a -> b) -> Maybe a -> Maybe b
--    fmap f (Just x) = Just $ f x
--    fmap f Nothing  = Nothing
-- <*>  :: Maybe (a -> b) -> Maybe a -> Maybe b
--    (Just f) <*> mx = fmap f mx
--    Nothing <*> mx = Nothing

applyMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f (Just v) = f v
applyMaybe f Nothing  = Nothing

{-
class Monad m where 
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    fail :: String -> m a

    (mx >> my) = mx >>= (\x -> my)
    fail msg = error msg

instance Monad [] where
    return :: a -> [a]
    return x = [x] -- x:[]

    (>>=) :: [a] -> (a -> [b]) -> [b]
    []     >>= f = [] 
    (x:xs) >>= f = (f x) ++ (xs >>= f)

instance Monad Maybe where
    return :: a -> Maybe a
    return x = Just x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (Just x) >>= f = f x
    Nothing  >>= f =  Nothing

    (>>) :: Maybe a -> Maybe b -> Maybe b 
    (Just x) >> my = my
    Nothing  >> my = Nothing

    fail str = Nothing

-}