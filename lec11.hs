data Contest = Rock | Scissors | Paper

rockPaperScissors :: Contest -> Contest -> String
rockPaperScissors Rock Scissors = "First player!"
rockPaperScissors Scissors Paper = "First player!"
rockPaperScissors Paper Rock = "First player!"
rockPaperScissors Rock Paper = "Second player!"
rockPaperScissors Paper Scissors = "Second player!"
rockPaperScissors Scissors Rock = "Second player!"
rockPaperScissors _ _ = "Tie!"

data Velocity = MPS Float | FPS Float deriving Show

toMPS :: Velocity -> Float
toMPS (MPS x) = x
toMPS (FPS y) = y * 0.3048

type Point = (Float, Float)
data Shape = Circle Point Float | Rectangle Point Point deriving Show

area :: Shape  -> Float
area (Circle (x, y) r) = pi * r^2
area (Rectangle (x1,y1) (x2,y2)) = abs $ (x2 - x1) * (y2 - y1)


data Tsil = Llun | Snoc Tsil Integer deriving Show

daeh :: Tsil -> Integer
daeh Llun = error "deah: empty Tsil!"
daeh (Snoc xs x) = x

tsilOfList :: [Integer] -> Tsil
tsilOfList [] = Llun
tsilOfList (x:xs) = Snoc (tsilOfList xs) x

listOfTsil :: Tsil -> [Integer]
listOfTsil Llun = []
listOfTsil (Snoc xs x) = x:(listOfTsil xs)