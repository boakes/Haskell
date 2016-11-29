import Vector

type Point = (Float, Float)
data Shape = Circle Float Point | Rectangle Point Point

area :: Shape -> Float 
area (Circle radius center) = pi * radius^2
area (Rectangle (x1,y1) (x2,y2)) = abs $ (x2-x1) * (y2-y1)

--data Maybe a = Nothing | Just a deriving Show

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

justOrZero :: Maybe Integer -> Integer 
justOrZero Nothing = 0
justOrZero (Just z) = z

headOrZero :: [Integer] -> Integer
headOrZero lst = 
	case myHead lst of
		Nothing -> 0
		(Just h) -> h

myMaximum :: [Integer] -> Integer
myMaximum [] = error "End of the world!!"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

myMaximumF lst =
	case myHead lst of
	Nothing -> error "end of stuff"
	(Just h) -> foldr (\x acc -> max x acc) h lst

