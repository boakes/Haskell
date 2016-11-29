--replicate :: Integer -> a -> [a]
myReplicate 0 y = [] 
myReplicate x y = let gift = (myReplicate (x-1) y) 
				  in y:gift  

myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip (x:xs) (y:ys) = let bae = (myZip xs ys)
					  in (x,y):bae
myZip _ _ = []

secondToLast :: [a] -> a 
secondToLast [x,y] = x
secondToLast [] = error "need more things"
secondToLast [x] = error "need more things"
secondToLast (x:xs) = secondToLast xs
					

--removeDuplicates from a sorted list
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:y:xs) = if(x==y) then removeDuplicates (y:xs) else x:(removeDuplicates(y:xs))
													  

-- remove duplicates w/o sorting

-- run length encoding [a] -> [(Integer,a)]