aNum = 5
aFloat = 7.2 
aString = "Hello"
aList = [1,2,3]
anotherList = ["Hello","World"]
aThirdList = [[5,9], [], [7]]
aTuple = ("Jacob", 19)
anotherTuple = (7,9,12.5)
aListOfTuples = [(a,b) | a <- [1..], b <- [1..]]
extraFunList = reverse [1..]

--removeUpper :: [Char] -> [Char]
removeUpper :: String -> String
removeUpper str = [ x | x <- str, not (x `elem` ['A'..'Z']) ]

addThree :: Integer -> Integer -> Integer -> Integer
addThree x y z = x + y + z

addTwo :: (Integer, Integer) -> Integer
addTwo tup = (fst tup) + (snd tup)

crossProduct :: [a] -> [b] -> [(a,b)]
--crossProduct :: [Integer] -> [Integer] -> [(Integer,Integer)]
--crossProduct :: [Char] -> [Char] -> [(Char, Char)]
--crossProduct :: String -> String -> [(Char, Char)]
--crossProduct :: [Char] -> [Integer] -> [(Char, Integer)]
crossProduct alst blst = [(a,b) | a <- alst, b <- blst]

-- show [7,3,2] turns whatever into a String