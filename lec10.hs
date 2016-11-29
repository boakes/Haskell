--Find the largest number under 100,000 that is divisible by 3829
largestDivisible = head $ (filter p) [100000, 99999..]
                where p x = (x `mod` 3829) == 0

-- lambda in haskell (\x -> x + 2) 5

myEven :: Integer -> Bool
myEven = (\x -> x `mod` 2 == 0)
--myEven x = x `mod` 2 == 0

easyLargestDiv = head $ filter (\x -> x `mod` 3829 == 0) [100000, 99999..]

--sillyValues = map (\tup -> fst tup + snd tup) [(1,2), (3,5), (6,3), (2,6), (2,5)]
sillyValues = map (\(x,y) -> x+y) [(1,2), (3,5), (6,3), (2,6), (2,5)]
sumPairs xs = map (\(x,y) -> x+y) xs


-- headyList lst = map (\(x:xs) -> x) lst   <---- doesn't allow nulls
headyList lst = map (\(x:xs) -> x) $ filter (not . null) lst
-- (not . null) . is the function composition 
-- alt way to check null headyList lst = map (\(x:xs) -> x) ( filter (\l -> not $ null l) lst )

--myFlip :: (a -> b -> c) -> b -> a -> c
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = (\b a -> f a b)

fold :: (a -> b -> b) -> b -> [a] -> b
fold f base [] = base
fold f base (x:xs) = f x (fold f base xs)

mySum lst = fold (\x acc -> x + acc) 0 lst

myElem :: Eq a => a -> [a] -> Bool
myElem y xs = fold (\x acc -> if x == y then True else acc) False xs