--multThree :: (Num a) => a -> a -> a -> a
multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

divideByTen = (/10)

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

nonNegative = max 0

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y):(myZipWith f xs ys)
myZipWith f _ _ = []

{-
multLists :: [Int] -> [Int] -> [Int]
multLists (x:xs) (y:ys) = (x*y):(multLists xs ys)
multLists _ _ = []
-}

multLists :: [Integer] -> [Integer] -> [Integer]
multLists = myZipWith (*)