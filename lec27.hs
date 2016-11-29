import System.IO
{-
 1) Create a datatype to represent a single input: a command from the user. A line can
 be a number, or an operation (+,-,/,*) 
-}

data Operation = Add | Mult | Div | Sub deriving Show
data Input =  Op Operation | Number Double deriving Show

readInput :: String -> Input 
readInput "+" = Op Add
readInput "*" = Op Mult
readInput "-" = Op Sub
readInput "/" = Op Div
readInput str = Number (read str)

data State = LFNum Double Operation | LFOp Double

update :: State -> Input -> Maybe State 
update (LFOp x) (Op y) = Just $ LFNum x y
update (LFOp x) (Number y) = Just $ LFOp y
update (LFNum x op) (Op op2) = Nothing 
update (LFNum x op) (Number y) = Just $ LFOp (x `opf` y) 
	where opf = case op of
					Add -> (+)
					Sub -> (-)
					Mult -> (*)
					Div -> (/)

{-
eval :: Double -> Operation -> Double -> Double
eval x Add y = x + y
eval x Mult y = x * y
eval x Sub y = x - y
eval x Div y = x / y

-}


instance Show State where
	show (LFOp x) = show x
	show (LFNum x op) = (show x) ++ (opstr op)
		where opstr Add = "+"
		      opstr Sub = "-"
		      opstr Mult = "*"
		      opstr Div = "/"


repl :: State -> IO()
repl state = 
	do 
	putStrLn (show state)
	putStr ">"
	str <- getLine
	let input = readInput str
	case update state input of
		Nothing -> 
		  do 
		  putStrLn "Invalid input. Try again."
		  repl state
		Just newState -> repl newState

{-
calcrepl :: State -> IO ()
calcrepl prev = do
	now <- getLine 
	let input = readInput now
	if (not (null now))
	then do 
		let newop = update2 (update prev input)
		putStrLn $ show newop
		calcrepl newop
	else return ()

update2 :: Maybe State -> State
update2 (Just x) = x
-}

main :: IO ()
main = repl $ (LFOp 0)