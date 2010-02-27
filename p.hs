data RealArbitrario = NoNeg [Int] [Int] Int
     		    | Neg [Int] [Int] Int
		    deriving (Eq)

parteEntera::[Int] -> Int -> Int -> Int
parteEntera [] base n = 0
parteEntera (x:xs) base n = x*(base^n) + parteEntera xs base (n+1)

parteFraccionaria::[Int] -> Int -> Int -> Double
parteFraccionaria [] base n = 0
parteFraccionaria (x:xs) base n = (fromIntegral x :: Double)*(1/(fromIntegral base^n :: Double ))
		  	      	  + parteFraccionaria xs base (n+1)

convertir::RealArbitrario -> RealArbitrario
convertir (NoNeg x y base) = Neg x y base
convertir (Neg x y base) = NoNeg x y base

showRA::RealArbitrario -> String
showRA (NoNeg x y base) = show ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1))
showRA (Neg x y base) = show (negate ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1)))

acarreo::Int -> Int -> Int
acarreo s base 
	  | s >= base = 1
	  | otherwise = 0

adicionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
adicionEnt [] x base ac = x
adicionEnt x [] base ac = x
adicionEnt (x:xs) (y:ys) base ac = 
				[(x + y + ac) `mod` base] ++ (adicionEnt xs ys base c)
			     	where c = acarreo (x + ac + y) base

adicionFrac::[Int] -> [Int] -> Int -> ([Int],Int)
adicionFrac [] x base = (x,0)
adicionFrac x [] base = (x,0)
adicionFrac (x:xs) (y:ys) base = 
	    	   	       ([(x + y + ac) `mod` base] ++ l, acarreo (x + y) base)
			       where (l,ac) = (adicionFrac xs ys base)
			     	       

sumaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
       	      	     	    	   	  	(NoNeg x y base1)
						where {
						      (y,z) = adicionFrac y1 y2 base1
						      ;x = adicionEnt x1 x2 base1 z
						      }

{-
sumaRA (Neg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = convertir (sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2))

-}