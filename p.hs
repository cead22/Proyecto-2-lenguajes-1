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

adicion::[Int] -> [Int] -> Int -> [Int]
adicion [] x base = x
adicion x [] base = x
adicion (x:xs) (y:ys) base = if x + y >= base
	       	      	     then 
			     	  if xs == []
			     	  then [x + y - base] ++ (adicion [1] ys base)
				  else [x + y - base] ++ (adicion ([(head xs) + 1] ++ (tail xs)) ys base)
			     else [x + y] ++ (adicion xs ys base)

convertir::RealArbitrario -> RealArbitrario
convertir (NoNeg x y base) = Neg x y base
convertir (Neg x y base) = NoNeg x y base

sumaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2) = --if base1 /= base2
								 --then throw "Error bases distintas"
								 --else 
								      let f = adicion (reverse (y:ys)) (reverse (w:ws)) base1
								      in
									if length f > max (length (y:ys)) (length (w:ws))
									then (NoNeg (adicion ([x + 1] ++ xs) (z:zs) base1) (tail (reverse f)) base1)
									else (NoNeg (adicion (x:xs) (z:zs) base1) f base1)

sumaRA (Neg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = convertir (sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2))