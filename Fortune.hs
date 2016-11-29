import RandomM
import System.Random
import System.IO
import System.Environment
import System.Console.GetOpt

data Flag = Repeat | Number Int deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
    Option ['r'] ["repeat"] (NoArg Repeat) "Interactively keep giving fortunes." ,
    Option ['n'] ["number"] (ReqArg (\str -> Number (read str)) "num") "Print <num> fortunes."
    ]

numberOfFlags :: [Flag] -> Int
numberOfFlags [] = 1
numberOfFlags ((Number x):_) = x
numberOfFlags (fl:fls) = numberOfFlags fls

takeMultiple :: Int -> [a] -> RandomM [a]
takeMultiple 0 lst = return []
takeMultiple n lst = do
        elem  <- randomElem lst 
        elems <- takeMultiple (n-1) lst
        return (elem:elems)

main :: IO ()
main = do
    args <- getArgs
    let (flags, others, errors) = getOpt Permute options args
    case others of
        [file] -> do
            handle <- openFile file ReadMode
            fortune_text <- hGetContents handle
            let fortunes = lines fortune_text
            gen <- getStdGen
            let fortune = evalRand (takeMultiple (numberOfFlags flags) fortunes) gen 
            putStrLn (unlines fortune)
            if (Repeat `elem` flags)
            then do 
                putStrLn "Do you want another?"
                response <- getLine
                if response `elem` ["Yes", "y", "yes", "Y", "Sure", "sure"]
                then main
                else return ()
            else return ()
        _ -> putStrLn $ usageInfo "Usage: Fortune [options] <filename> <num>" options

