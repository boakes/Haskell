import System.Random

{-
tripleNum :: StdGen -> ((Integer, Integer, Integer), StdGen) 
tripleNum gen = let 
        (x, gen') = randomR (0,10) gen
        (y, gen'') = randomR (25,50) gen'
        (z, gen3) = randomR (50,75) gen''
	in ((x,y,z),gen3)
-}

data RandomVal a = R (StdGen -> (a, StdGen))

evalRand :: RandomVal a -> StdGen -> a 
evalRand (R f) gen = fst (f gen)

randomRangeV :: (Integer, Integer) -> RandomVal Integer
randomRangeV range = R (\gen -> randomR range gen)

instance Monad RandomVal where
	--return :: a -> RandomVal a
	return av = R (\gen -> (av, gen))
	--(>>=) :: RandomVal a -> (a -> RandomVal b) -> RandomVal b
	-- r1 :: StdGen -> (a, Stdgen)
	-- f :: (a -> RandomVal b)
	(R r1) >>= f = R (\gen -> let 
                                  (av, gen') = r1 gen
                                  (R r2) = f av -- StdGen -> (b, StdGen)
                                  (bv, gen'') = r2 gen'
                              in (bv, gen''))

	-- (>>) :: RandomVal a -> randomVal b -> randomVal b
	-- f1 :: StdGen -> (a, StdGen) 
	-- f2 :: StdGen -> (b, StdGen) 
	--(R f1) >> (R f2) = R (\gen -> )
	(R r1) >> (R r2) = R (\gen -> let
                                     (av,gen') = r1 gen
                                     (bv, gen'') = r2 gen'
                                  in (bv, gen''))


tripleNum :: RandomVal (Integer,Integer,Integer)
tripleNum = do
   x <- randomRangeV (0,10)
   y <- randomRangeV (25,50)
   z <- randomRangeV (50,75)
   return (x,y,z) 