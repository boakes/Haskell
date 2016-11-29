import Data.Char

main = do
    putStrLn "Hello, what's your first name?"
    firstName <- getLine
    if firstName == ""
    	then return ()
	else do
		putStrLn "What's your last name?"
		lastName <- getLine
		let bigFirstName = map toUpper firstName
		let bigLastName = map toUpper lastName
		putStrLn ("hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?")
		getLine
		return ()