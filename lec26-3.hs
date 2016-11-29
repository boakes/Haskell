import Data.Char

repeatLine :: String -> IO()
repeatLine prev = do
	line <- getLine 
	putStrLn prev
	if line == ""
		then return ()
	else do
		repeatLine line 

main :: IO ()
main = repeatLine ""