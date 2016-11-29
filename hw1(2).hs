collatzEsque x = if x < 0
                 then x * (-2)
                 else (x-10) * (-1)

firstTwo xs = take 2 xs

firstTwoTup xs = (head xs, last (take 2 xs))

evenUpTo y = [x | x <- [0,2..y]]

divisiblePairs xs = [(y,x) | x <- xs, y <- xs, x /= y, (x `mod` y) == 0]

sOfL len strs = [some_strings | some_strings <- strs, len == length some_strings]
stringsOfLengths lens strs = [(a_length,(sOfL a_length strs)) | a_length <- lens]