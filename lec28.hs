import Control.Monad

repl :: [String] -> IO ()
repl lines = do 
	line <- getLine
	let revLine = reverseWords line
	if (not (null line))
	then do let newLines = lines++[revLine]
			printLines newLines
			repl newLines
	else return ()

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

printLines :: [String] -> IO ()
printLines [] = return ()
printLines (str:strs) = do putStrLn str
						   printLines strs

main :: IO ()
main = repl []