import Data.Char

incList :: [Integer] -> [Integer]
incList [] = []
incList (x:xs) = (x+1) : (incList xs)

--using toUpper 
upperString :: [Char] -> [Char]
upperString [] = []
upperString (x:xs) = (toUpper x) : (upperString xs)

--threeIfy [[1,2], [], [9]] = [[3,1,2], [3],[3,9]]
threeIfy :: [[Integer]] -> [[Integer]]
threeIfy [] = []
threeIfy (x:xs) = (3:x) : (threeIfy xs)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

nonNegatives :: [Integer] -> [Integer]
nonNegatives [] = []
nonNegatives (x:xs) = if x>= 0
                      then x:(nonNegatives xs)
                      else nonNegatives xs
--nonNegatives xs = [x | x <- xs, x>0]

evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs) = if x `mod` 2 == 0
               then x:(evens xs)
               else evens xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs) = if p x
                      then x:(myFilter p xs)
                      else myFilter p xs

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + (sumList xs) 

multList :: [Integer] -> Integer
multList [] = 1
multList (x:xs) = x * (multList xs)

fold :: (a -> b -> b) -> b -> [a] -> b
fold f base [] = base
fold f base (x:xs) = f x (fold f base xs)

