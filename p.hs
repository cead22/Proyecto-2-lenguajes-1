data RealArbitrario = NoNeg [Int] [Int] Int
     		    | Neg [Int] [Int] Int
		      deriving (Eq)

parteEntera ::[Int] -> Int -> Int -> Int
parteEntera [] base n = 0
parteEntera (x:xs) base n = x * (base^n) + parteEntera xs base (n + 1)

parteFraccionaria::[Int] -> Int -> Int -> Double
parteFraccionaria [] base n = 0
parteFraccionaria (x:xs) base n = (fromIntegral x :: Double) * (1 / (fromIntegral base^n :: Double ))
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
adicionEnt [] [] base ac = [ac]
adicionEnt [] (x:xs) base ac = [x + ac] ++ xs
adicionEnt (x:xs) [] base ac = [x + ac] ++ xs
adicionEnt (x:xs) (y:ys) base ac = 
    [(x + y + ac) `mod` base] ++ (adicionEnt xs ys base c)
	where c = acarreo (x + ac + y) base

adicionFrac::[Int] -> [Int] -> Int -> ([Int],Int)
adicionFrac [] [] base = ([],0)
adicionFrac [] x base = (x,0)
adicionFrac x [] base = (x,0)
adicionFrac (x:xs) (y:ys) base = 
    ([(x + y + ac) `mod` base] ++ l, acarreo (x + y + ac) base)
	where (l,ac) = (adicionFrac xs ys base)
			     	       

sumaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    (NoNeg x y base1)
	where {
	  (y,z) = adicionFrac y1 y2 base1 ;
	  x = adicionEnt x1 x2 base1 z
	}
sumaRA (Neg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = 
    convertir (sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2))

limpia::[Int] -> [Int]
limpia [] = []
limpia (x:xs) = if x == 0
                then limpia xs
                else (x:xs)
          
limpiar::RealArbitrario -> RealArbitrario
limpiar (NoNeg x y base) = NoNeg (reverse (limpia (reverse x))) (reverse (limpia (reverse y))) base

mayor::RealArbitrario -> RealArbitrario -> Bool
mayor (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | reverse x1 > reverse x2 = True
    | reverse x1 == reverse x2 && y1 >= y2 = True
    | otherwise = False

prestamo::Int -> Int
prestamo s
    | s < 0 = 1
    | otherwise = 0

sustraccionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
sustraccionEnt [] [] base pr = []
sustraccionEnt [] (x:xs) base pr = [x - pr] ++ xs
sustraccionEnt (x:xs) [] base pr = [x - pr] ++ xs
sustraccionEnt (x:xs) (y:ys) base pr = 
    [(x - y - pr) `mod` base] ++ (sustraccionEnt xs ys base c)
	where c = prestamo (x - y - pr)

sustraccionFrac::[Int] -> [Int] -> Int -> ([Int],Int)
sustraccionFrac [] [] base = ([],0)
sustraccionFrac [] x base = (x,1)
sustraccionFrac x [] base = (x,0)
sustraccionFrac (x:xs) (y:ys) base = 
    ([(x - y - pr) `mod` base] ++ l, prestamo (x - y - pr))
	where (l,pr) = (sustraccionFrac xs ys base)

rellenar::[Int] -> Int -> [Int]
rellenar x t = x ++ [0 | z <- [1..t]]

sustraccionRA::RealArbitrario -> RealArbitrario -> RealArbitrario
sustraccionRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    if y1 >= y2 
    then
        let (y,_) = (sustraccionFrac (rellenar y1 ((length y2) - (length y1))) y2 base1)
        in NoNeg (sustraccionEnt x1 x2 base1 0) y base1
    else
        let (y,_) = (sustraccionFrac (rellenar ([1]++y2) ((length y1) - (length y2))) y1 base1)
        in NoNeg (sustraccionEnt x1 x2 base1 1) (tail (reverse y)) base1


restaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
restaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | (mayor num1 num2) = sustraccionRA num1 num2
    | otherwise = convertir (sustraccionRA num2 num1)
    where {
      num1 = limpiar (NoNeg x1 y1 base1) ;
      num2 = limpiar (NoNeg x2 y2 base2)
    }
