-- Data Type of (2*5) - (10/3)  
--		      - (mult 2 5) (div 10 3) 

data AExp = Add AExp AExp | Mult AExp AExp | Div AExp AExp | Sub AExp AExp | N Float deriving Show

eval :: AExp -> Float
eval xp = case xp of 
	 		  Add lhs rhs -> (eval lhs) + (eval rhs) 
			  Mult lhs rhs -> (eval lhs) * (eval rhs)
 			  Sub lhs rhs -> (eval lhs) - (eval rhs)
 			  Div lhs rhs -> (eval lhs) / (eval rhs)
 			  N x -> x

--ParsePrefix of ["+","2","-","123","7"] should be Add (N 2) (Sub (N 123) (N 7))
parsePrefix :: [String] -> AExp
parsePrefix = 

data Op = MultOp | AddOp | DivOp | SubOp
data AExpB = OpExp Op AExpB AExpB | Num Float
evalB :: AExp -> Float
evalB (OpExp op lhs rhs) = (evalB lhs) `func` (evalB rhs)
     where func = case op of
     			 	MultOp -> (*)
     			 	AddOp -> (+)
     			 	DivOp -> (/)
     			 	SubOp -> (-)
evalB (Num x) = x