cylinder r h = let
		sideArea = 2 * pi * r * h
		topArea = pi * r^2
	in sideArea + 2*topArea

sumList [] = 0
sumList (x:xs) = x + (sumList xs)

myMaxumum (x:[]) = x --could say myMaximum [x]
myMaximum (x:xs) = max x (myMaximum xs)

myElem :: Ord a => a -> [a] -> Bool
myElem a []  = False
myElem a (x:xs) = a == x || myElem a xs

{- 1. If problem "simple enough" then solve directly 
   2. Otherwise
		a. break into simpler sub-problem
		b. Recursion Faeries
			solve faster sub-problems
		c. Compose solutions into a solution of overall problem.
-}

quickSort [] = []
quickSort [a] = [a]
quickSort (x:xs) =let 
		ltX = [a | a <- xs, a <= x]
		gtX = [a | a <- xs, a > x]
		sortedLtX = quickSort ltX
		sortedGtX = quickSort gtX
	in sortedLtX ++ [x] ++ sortedGtX