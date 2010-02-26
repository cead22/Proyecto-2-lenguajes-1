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

showRA::RealArbitrario -> String
showRA (NoNeg x y base) = show ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1))
showRA (Neg x y base) = show (negate ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1)))


