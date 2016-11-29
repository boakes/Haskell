tellFortune :: String -> String 
tellFortune ('B':_) = "You wah"
tellFortune ('T':_) = "things can happen"
tellFortune _  = "maybe you will die or live"


main :: IO ()
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Here is your fortune, " ++ name ++ "."
    putStrLn (tellFortune name)
    putStrLn "Press enter to continue."
    getLine
    return ()