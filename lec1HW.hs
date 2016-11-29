min3 x y z = min x (min y z)
max3 x y z = max x (max y z)
middle x y z = (x+y+z)- (min3 x y z) - (max3 x y z)
absvalue nums = [if x < 0 then (-1) * x else x | x <- nums]
trianglesToTen = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2+b^2 == c^2, a < b]
trianglesToX max = [(a,b,c) | c <- [1..max], b <- [1..max], a <- [1..max], a^2+b^2 == c^2, a < b]