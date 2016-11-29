--Collaboration with Charles Marcus Whitten and Brett Skogman

import CSCICourses

--Problem 1
data Group = Design| Applications | Systems deriving (Show,Eq)

--Problem 2

type CID = String
type Hours = Int
data Course = G CID Hours Group [CID] | S CID Hours [CID] deriving (Show,Eq)
preReq (G _ _ _ pr) = pr
preReq (S _ _ pr) = pr

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
data Curriculum = Curriculum [CID] [Group] Int deriving Show 

-- Problem 5 

csciBach :: Curriculum 
csciBach = Curriculum ["1120","1320","1321","2320","2321","2322","3320","3321","3322"] [Design,Applications,Systems] 49

csciC2M :: Curriculum
csciC2M = Curriculum ["1120","1320","1321","1323","2320"] [] 34

-- Problem 6 
data Transcript = Transcript [CID] [Group] Int

frances :: Transcript
frances = Transcript ["1321","1120"] [] 4

sally :: Transcript
sally = Transcript ["1120","1320","1321","1323","2320"] [Design] 34 

-- Problem 7
canTake :: Transcript -> Course -> Bool
canTake trans cours = helpCan( 

-- Problem 8
hoursTaken :: Transcript -> Int

--Problem 9 
groupsTaken :: Transcript -> [Group]

--Problem 10
canGraduate :: Curriculum -> Transcript -> Bool

-- Problem 11 
missing :: Curriculum -> Transcript -> String 
