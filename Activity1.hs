prodList :: [Integer] -> Integer
prodList lst = foldr (\x acc -> x * acc) 1 lst 

lengthsOfStrings :: [String] -> [Int]
--lengthsOfStrings strs = map length strs 
lengthsOfStrings strs = foldr (\x acc -> (length x):acc) [] strs

reverseFold :: [a] -> [a]
reverseFold lst = foldl (\acc x -> x:acc) [] lst
--reverseFold lst = foldr (\x acc -> acc++[x]) [] lst

sumPositive :: [Integer] -> Integer
sumPositive lst = foldr(+) 0 $ filter(>=0) lst 
sumPositive lst = foldr(\x acc -> if x>=0 then x+acc else acc) 0 lst 



foldyFilter :: (a -> Bool) -> [a] -> [a]
foldyFilter p lst = foldr(\x acc -> if p x then x:acc else acc) [] lst

-- myPositives :: [Integer] -> [Integer]


partByRec y [] = ([],[])
partByRec y (x:xs) =
	let (lts, gtes) = partByRec y xs 
	in if x >= y
		then (lts,x:gtes)
		else (x:lts,gtes)
partitionBy :: Ord a => a -> [a] -> ([a],[a])
partitionBy pvt lst =  foldr(\x (lts, gtes) -> if x < pvt 
											   then (x:lts,gtes)  
									   	       else (lts,x:gtes)) ([],[]) lst 
partitionByF y lst = (filter (<y) lst, filter (>=y) lst)