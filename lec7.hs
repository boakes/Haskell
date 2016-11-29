occurancesOfX :: Eq a => a -> [a] -> Integer
occurancesOfX x [] = 0
occurancesOfX x (y:ys) = 
	let gift = occurancesOfX x ys
	in if x == y then 1 + gift else gift

{- occurancesOfX x (y:ys) = 
	| x==y = 1 + (occurancesOfX x ys)
	| otherwise = occurancesOfX x ys
-}
occurancesOfHead :: Eq a => [a] -> Integer
occurancesOfHead lst =  occurancesOfX (head lst) lst

{- input: a list of tasks represented as (hours,min)
   output: how many hours to schedule.
-}

{-
	This way couldn't be done correctly 
	superCompSched :: [(a,b)] -> Integer
	superCompSched [] = 0
	superCompSched ((h,m):tasks) = let gift = superCompSched tasks
-}

sumTimes :: [(Integer,Integer)] -> (Integer, Integer)
sumTimes [] = (0,0)
sumTimes ((h,m) : times) = let 
		(hours,mins) = sumTimes times
		tMins = m+mins
	in if tMins > 60 
	   then (hours+h+1,tMins-60)
	   else (hours+h,tMins)
	--(h+hours+tMins `div` 60,tMins `mod` 60)

hoursToSchedule :: [(Integer,Integer)] -> Integer
hoursToSchedule tasks = 
	let (hours,mins) = sumTimes tasks
	in if mins > 0 
		then (hours+1)
		else hours

removeDups :: Eq a => [a] -> [a]
removeDups lst = removeDupsAnd [] lst

removeDupsAnd :: Eq a => [a] -> [a] -> [a]
removeDupsAnd seen [] = []
removeDupsAnd seen (x:xs) = 
	if x `elem` seen
	then removeDupsAnd seen xs
	else x:(removeDupsAnd (x:seen) xs)

mostlySorted :: Ord a => [a] -> Bool
mostlySorted lst = let 
	aux count [] = not (count < 2)
	aux count [x] = not (count < 2)
	aux count (x:xs) = 
				if x <= head xs
				then aux count xs
				else aux (count+1) xs
	in aux 0 lst
{-
	let aux [] = 0
		aux(x:xs) = 
			if x <= head xs
			then aux xs
			else 1+(aux xs)
	in not (aux lst > 2)

mostlySorted [] = True
mostlySorted (x:xs) = 
	let gift = mostlySorted xs
	in if x <= head xs
		then gift
		else 
-}