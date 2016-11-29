-- Collab with with Brett Skogman & Marcus Whitten

concatenate :: [String] -> String
concatenate strs = foldr (\x acc -> x++acc) [] strs

sorted :: Ord a => [a] -> Bool 
sorted [] = True
sorted [x] = True
sorted lst@(x:xs) = foldr(\x acc -> if (x >= head(xs))
								    then sorted xs 
								    else False) True lst 

rangeMin (x:xs) = foldr (min) x xs 
rangeMax (x:xs) = foldr (max) x xs
range :: Ord a => [a] -> (a,a)
range [x] = (x,x)
range (x:xs) = (min x (rangeMin xs), max x (rangeMax xs)) 

flatmap :: (t -> [a]) -> [t] -> [a]
flatmap _ [] = []
flatmap f (x:xs) = f x ++ (flatmap f xs)


exists :: (t-> Bool) -> [t] -> Bool
exists p xs =
	let lst = filter p xs
	in if(length lst /= 0)
	   then True
	   else False

forall :: (t -> Bool) -> [t] -> Bool
forall p xs = 
	let lst = filter p xs
	in if(length lst == length xs)
		then True
		else False

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p xs = (filter p xs, filter (not . p) xs)