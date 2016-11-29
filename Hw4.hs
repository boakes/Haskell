--Collaboration with Charles Marcus Whitten and Brett Skogman

import CSCICourses
import Data.List

--Problem 1
data Group = Design| Applications | Systems | None deriving (Show, Eq) 

--Problem 2

type CID = String
type Hours = Int
data Course = G CID Hours Group [CID] | S CID Hours [CID] deriving Show

-- Problem 3
readCourse :: String -> Course 
readCourse line =  
	let (a:b:c:ds) = words line 
	in case () of
	 () | c == "None" -> S a (read b) ds
	    | c == "Design" -> G a (read b) Design ds
	    | c == "Applications" -> G a (read b) Applications ds
	    | c == "Systems" -> G a (read b) Systems ds 

-- Problem 4
data Curriculum = C [CID] [Group] Int deriving Show 

-- Problem 5 

csciBach :: Curriculum 
csciBach = (C ["1120","1320","1321","1323","2320","2321","2322","3320","3321","3322"] [Design,Applications,Systems] 49)

csciC2M :: Curriculum
csciC2M = (C ["1120", "1320", "1321", "1323", "2320"] [] 34)

-- Problem 6 
data Transcript = T [Course] 

frances :: Transcript
frances = (T [(readCourse ("1321 3 None 1320")),(readCourse ("1120 1 None 1320"))])

sally :: Transcript
sally = T [(readCourse ("4444 21 Design 1320")), (readCourse ("1120 1 None 1320")), (readCourse ("1320 3 None")), (readCourse ("1321 3 None 1320")), (readCourse ("1323 3 None 1320")), (readCourse ("2320 3 None 1321 1120"))]

-- Problem 7
canTake :: Transcript -> Course -> Bool
canTake script cour = True

-- Problem 8
helperG :: Course -> Int
helperG (G a b c ds) = b
helperG (S a b ds) = b

helperLst :: [Course] -> Int
helperLst [] = 0
helperLst (x:[]) = helperG x
helperLst (x:xs) = helperG x + helperLst xs

hoursTaken :: Transcript -> Int
hoursTaken (T lst) = helperLst lst

--Problem 9 
helperGC :: Course -> [Group]
helperGC (G a b c ds) = [c]
helperGC (S a b ds) = [None]

helperLstC :: [Course] -> [Group]
helperLstC ([]) = []
helperLstC (x:xs) = helperGC x ++ helperLstC xs

groupsTaken :: Transcript -> [Group]
groupsTaken (T lst) = nub (helperLstC lst)

--Problem 10
--canGraduate :: Curriculum -> Transcript -> Bool

-- Problem 11 
--missing :: Curriculum -> Transcript -> String 