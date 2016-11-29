data Player = Red | Blk | DAN deriving (Show, Eq, Read)
data GameBoard = State {turn::Int, board::[[Player]], height::Int, width::Int} deriving Show

data GameTree idx val board = Node idx val board [GameTree Int Int GameBoard] | Leaf | Root GameBoard [GameTree Int Int GameBoard] deriving Show 


--Creates a new empty GameBoard
startGame :: Int -> Int -> GameBoard
startGame h w  = State 0 (blist w) h w 
    where 
          blist 1 = [[]]
          blist sz = []:(blist (sz-1)) 

--returns what a blank list from startGame would look like
freshList :: Int -> Int -> [[Player]]
freshList h w  = blist w
    where 
          blist 1 = [[]]
          blist sz = []:(blist (sz-1))

getBoard :: GameBoard -> [[Player]]
getBoard (State turn board height width) = board

turnChange :: Int -> Int
turnChange x = x+1 

getPlayer :: Int -> Player
getPlayer turn  = case (turn`mod`2+1) of
    1 -> Red
    2 -> Blk 

gbg :: GameBoard -> Player
gbg (State turn board height width) = getPlayer turn

insertCol :: [[Player]] -> Int -> Player -> [[Player]]
insertCol (x:xs) 0 trn = ((trn:x):xs)
insertCol (x:xs) idx trn = x : (insertCol xs (idx-1) trn)

--subset from Data.List
hasSequence ::[Player] -> [Player] -> Int -> Bool
hasSequence pattern input count =  
     if count > ((length pattern)-1) then True
     else if (length input) == 0 then False
     else if (head input) == pattern!!count then hasSequence pattern (tail input) (count+1)
     else hasSequence pattern (tail input) (0)
{- 
    if count > ((length pattern)-1) then True
    else if (length input) == 0 then False
    else if (head input) == pattern!!count then hasSequence pattern (tail input) (count+1)
    else hasSequence pattern (tail input) (0)
-}

checkPat :: [[Player]] -> Player 
checkPat lst = 
    if      (any (\x->hasSequence [Red,Red,Red,Red] x 0) lst) then Red
    else if (any (\x->hasSequence [Blk,Blk,Blk,Blk] x 0) lst) then Blk
    else DAN

setUpDiag :: GameBoard -> [[Player]]    
setUpDiag (State turn [] height width) = freshList height width
setUpDiag (State turn lst height width) = takeWhile (not . null) $
                   zipWith(++) (map (:[]) (head lst) ++ (freshList height width)) ([]:setUpDiag (State turn (tail lst) height width))

--Need to fix this functions
checkWinner :: GameBoard -> Player
checkWinner (State turn board height width) = 
    if turn <= 7 then DAN
    else if horz /= DAN then horz
    else if vert /= DAN then vert
    else if diag /= DAN then diag
    else if diagRev /= DAN then diagRev
    else DAN
        where
            horz = checkPat (setUpHorz (State turn board height width) 0 0 (freshList height width))
            vert = checkPat board
            diag = checkPat (setUpDiag (State turn board height width))
            diagRev = checkPat (setUpDiag (State turn (reverse board) height width))

--always call with 0 0 for x y and a fresh list for cur    
setUpHorz :: GameBoard -> Int -> Int -> [[Player]] -> [[Player]]
setUpHorz (State turn board height width) x y cur = 
    if y>(height-1) then map reverse cur
    else if x>(width-1) then setUpHorz (State turn board height width) (0) (y+1) (cur)
    else if length (board !! x) == 0 then setUpHorz (State turn board height width) (x+1) (y) (insertCol cur y DAN)
    else if length (board !! x) < (height-y) then setUpHorz (State turn board height width) (x+1) (y) (insertCol cur y DAN)    
    else if length (board !! x) >= (height-y) then setUpHorz (State turn board height width) (x+1) (y) (insertCol cur y (reverse(board!!x) !! (height-y-1)))
    else map reverse cur


mapPrint :: [[Player]] -> IO()
mapPrint lst = mapM_ print lst

printGame :: GameBoard -> IO()
printGame (State turn board height width) = mapPrint (setUpHorz(State turn board height width) 0 0 (freshList width height))    

{-
isValid :: [[Player]] -> Int -> Int -> Int -> Bool
isValid lst idx h w = 
    if(idx < 0 || idx > (w -1)) then False
    else if(length (getCol lst idx) > (h - 1)) then False
    else True
-}

isValid :: GameBoard -> Int -> Bool
isValid (State turn board h w) idx = 
    if(idx < 0 || idx > (w -1)) then False
    else if(length (board!!idx) > (h - 1)) then False
    else True

move :: Int -> GameBoard -> GameBoard
move index (State turn board height width) = (State nextTurn lst h w)
        where 
            lst = if (isValid (State turn board height width) index ) then (insertCol board index (getPlayer turn)) else board
            nextTurn = if (isValid (State turn board height width) index ) then turnChange(turn) else turn
            h = height
            w = width 

gameToString :: GameBoard -> String
--gameToString (State turn lst height width) = (show height) ++ "\n" ++ (show width) ++ "\n" ++ (show turn)  ++ "\n" ++  (concatMap (++"\n") (map show lst))
gameToString (State turn lst height width) = (show height) ++ "\n" ++ (show width) ++ "\n" ++ (show turn)  ++ "\n" ++  ((show lst))

gameToFile :: GameBoard -> FilePath -> IO()
gameToFile gb fp= writeFile (fp) (gameToString gb)

stringToGame :: String -> GameBoard
stringToGame gameString = (State (read turn :: Int) (read lst :: [[Player]]) (read height :: Int) (read width :: Int))
    where [height, width, turn, lst] = lines gameString

fileToGame :: FilePath -> IO GameBoard
fileToGame coolguy = do 
    text <- readFile coolguy
    return (stringToGame text) 


getAllMoves :: GameBoard -> Int -> [Int] 
getAllMoves gb (-1) = []
getAllMoves gb col = if (isValid gb col)
                      then col:(getAllMoves gb (col-1))
                      else getAllMoves gb (col-1)                     

--fillGameTree :: GameTree -> Int -> [Int] -> GameTree
fillGameTree (Node x y z chld) _ [] = Node x y z (Leaf:chld)
fillGameTree (Node mv num gb chld) width valmoves = Node mv num gb (thng++chld)
                                              where thng = map (\(y,z) -> fillGameTree (Node z 0 y []) width (getAllMoves y width)) (map (\x -> ((move x gb),x)) valmoves)

createTree gb = (Root gb (map (\(y,z) -> fillGameTree (Node z 0 y []) (getWidth gb) (getAllMoves y (getWidth gb))) (map (\x -> ((move x gb),x)) (getAllMoves gb (getWidth gb)))))

getWidth (State turn lst height width) = width



--fillTreeVal :: GameTree -> GameTree
{-fillTreeVal (Node mv val gb [Leaf]) = if (checkWinner gb == Red) then Node mv (-1) gb [Leaf] 
                                      else if (checkWinner gb == Blk) then Node mv (1) gb [Leaf] 
                                      else Node mv 0 gb [Leaf] 
-}

--getValue :: GameTree -> Int
getValue (Node _ v _ _) = v
getTup (Node i v _ _) = (i,v)
--blkTreeFill :: GameTree -> GameTree
{-blktreeFill (Node i v gb [Leaf]) = if checkWinner gb == Red then (Node i (-1) gb [Leaf]) 
                                   else if checkWinner gb == Blk then (Node i (1) gb [Leaf]) 
                                   else (Node i 0 gb [Leaf])
-}

fixTreeVal (Root gb chld) = Root gb (map (\x ->fixTreeVal x) chld)   
fixTreeVal (Node i v gb [Leaf]) = if checkWinner gb == Red then (Node i (-1) gb [Leaf]) 
                                 else if checkWinner gb == Blk then (Node i (1) gb [Leaf]) 
                                 else (Node i 0 gb [Leaf])
                                 --else if (checkWinner gb) == DAN then (Node i 0 gb [Leaf])
fixTreeVal (Node i v gb chld) =  if (gbg gb) == Red then (Node i (minimum (map (\y->getValue y) (map (\x -> fixTreeVal x) chld))) gb chld)
                                 else (Node i (maximum (map (\y->getValue y) (map (\x -> fixTreeVal x) chld))) gb chld)
                
getChildren (Node idx val board chld) = chld

mintup (x:xs) = minTail x xs
  where minTail curMin [] = curMin
        minTail (m,n) (p:ps)
          | n < (snd p) = minTail (m,n) ps
          | otherwise = minTail p ps 

maxtup (x:xs) = maxTail x xs
  where maxTail curmax [] = curmax
        maxTail (m,n) (p:ps)
          | n < (snd p) = maxTail (m,n) ps
          | otherwise = maxTail p ps 

--bestMove :: GameBoard -> Int
bestMove (State turn board height width) = 
  let (Root gb chld) = fixTreeVal $ (createTree (State turn board height width))
  in if (getPlayer turn) == Red
     then fst $ mintup $ (map (\x -> getTup x) chld)  
  else fst $ maxtup $ (map (\x -> getTup x) chld)

--State {turn = 11, board = [[Blk,Red,Blk,Red],[Red,Red,Blk,Red],[Blk],[Red,Blk]], height = 4, width = 4}
--State {turn = 6, board = [[Blk,Red], [Blk,Red], [Blk,Red], []], height = 4, width = 4}
  --if (getPlayer turn) == Red then return index of the minium value in the tree's immediate children
  --else return index of the maximum value in the tree's immediate children

--(min (map (\y->getValue y) (map (\x -> blktreeFill x) chld)))
--
{-
qp :: Int -> Int
qp a = qp a
-}
                                  --lowest value elevate



                                  



                                                    --fillGameTree (Node (head valmoves) 0 (move (head valmoves) gb) []) (tail valmoves) ++ chld)

--Needs to be mapped or something, currently nesting valid moves


{-

-}                    
