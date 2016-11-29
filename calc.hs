-- 1) Create a datatype to represent a single input: a command from the user. A line can be a
-- number, or an operation (+, -, /, exponent)

data Operation = Add | Mult | Div | Sub | Exp deriving Show
data Input = Number Double | Op Operation deriving Show-- Add | Mult | Div | Sub | Exp

-- 1b)  Write a function readInput :: String -> Input

readInput :: String -> Input
readInput "+" = Op Add
readInput "-" = Op Sub
readInput "*" = Op Mult
readInput "/" = Op Div
readInput "^" = Op Exp
readInput x = Number (read x)

-- 2) Not all lines are valid inputs at every point. Encode the possible states of the calculator in
-- a second data type.
data State = LFNum Double Operation | LFOp Double
--
-- 3) Write an update function
update :: State -> Input -> Maybe State
update = undefined
--
--
-- 4) Create a custom show instance for state that looks nice.
instance Show State where
    --show :: State -> String
    show (LFOp x) = show x
    show (LFNum x op) = (show x) ++ (showOp op)
        where showOp Add = "+"
              showOp Sub = "-"
              showOp Mult = "*"
              showOp Div = "/"
              showOp Exp = "^"


-- 5) Create an IO interface: read in a line, update the state, display the state, go back to the
-- beginning.
--
-- 6) Optional IO Improvements:
--      a) 'exit' should exit the program.
--      b) MR and MS to store and recall a singel value.
--      c) Parenthesis.
