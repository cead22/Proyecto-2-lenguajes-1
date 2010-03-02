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

acarreoSum::Int -> Int -> Int
acarreoSum s base 
    | s >= base = 1
    | otherwise = 0

adicionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
adicionEnt [] [] base ac = [ac]
adicionEnt [] (x:xs) base ac = [x + ac] ++ xs
adicionEnt (x:xs) [] base ac = [x + ac] ++ xs
adicionEnt (x:xs) (y:ys) base ac = 
    [(x + y + ac) `mod` base] ++ (adicionEnt xs ys base c)
	where c = acarreoSum (x + ac + y) base

adicionFrac::[Int] -> [Int] -> Int -> ([Int],Int)
adicionFrac [] [] base = ([],0)
adicionFrac [] x base = (x,0)
adicionFrac x [] base = (x,0)
adicionFrac (x:xs) (y:ys) base = 
    ([(x + y + ac) `mod` base] ++ l, acarreoSum (x + y + ac) base)
	where (l,ac) = adicionFrac xs ys base
			     	       
adicion::[Int] -> [Int] -> [Int] -> [Int] -> Int -> ([Int],[Int])
adicion x1 y1 x2 y2 base =
    (x,y)
    where {
      (y,z) = adicionFrac y1 y2 base ;
      x = adicionEnt x1 x2 base z
    }

sumaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    (NoNeg x y base1)
	where (x,y) = adicion x1 y1 x2 y2 base1

sumaRA (Neg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = 
    convertir (sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2))

limpia::[Int] -> [Int]
limpia [] = []
limpia (x:xs) = if x == 0
                then limpia xs
                else (x:xs)
          
limpiar::RealArbitrario -> RealArbitrario
limpiar (NoNeg x y base) = NoNeg (reverse (limpia (reverse x))) (reverse (limpia (reverse y))) base

-- recibe dos listas del mismo tamano, al derecho
mayorEstrictoPorNumero::[Int] -> [Int] -> Bool
mayorEstrictoPorNumero [] [] = False
mayorEstrictoPorNumero (x:xs) (y:ys)
    | x > y = True
    | x < y = False
    | otherwise = mayorEstrictoPorNumero xs ys

-- recibe listas al derecho y limpias
mayorEstricto::[Int] -> [Int] -> Bool
mayorEstricto x y
    | length x > length y = True
    | length x == length y = mayorEstrictoPorNumero x y
    | otherwise = False

{-
mayor::RealArbitrario -> RealArbitrario -> Bool
mayor (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | reverse x1 > reverse x2 = True
    | reverse x1 == reverse x2 && y1 >= y2 = True
    | otherwise = False
-}

mayor::RealArbitrario -> RealArbitrario -> Bool
mayor (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | mayorEstricto (reverse x1) (reverse x2) = True
    | reverse x1 == reverse x2 && mayorOIgual y1 y2 = True
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

acarreoMult::Int -> Int -> Int
acarreoMult s base
    | s >= base = s `div` base
    | otherwise = 0

multiNumEnt::[Int] -> Int -> Int -> Int -> [Int]
multiNumEnt [] n base ac = [ac]
multiNumEnt (x:xs) n base ac = 
    [(x * n + ac) `mod` base] ++ (multiNumEnt xs n base c)
    where c = acarreoMult (x * n + ac) base

multi::[Int] -> [Int] -> Int -> Int -> [Int]
multi x [] base c = []
multi x (y:ys) base c =
    adicionEnt ([ 0 | i <- [1..c]] ++ a) b base 0
    where {
      a = multiNumEnt (reverse x) (last (y:ys)) base 0 ;
      b = multi x (init (y:ys)) base (c + 1)
    }

multRA::RealArbitrario -> RealArbitrario -> RealArbitrario
multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) =
    NoNeg y (reverse x) base1
    where {
      (x,y) = splitAt ((length y1) + (length y2)) z ;
      z = multi ((reverse x1) ++ y1) ((reverse x2) ++ y2) base1 0
    }

divSimple::[Int] -> Int -> Int -> Int -> ([Int],Int)
divSimple [] n base r = ([],r)
divSimple (x:xs) n base r =
    (q,b)
    where {
      q = [(r*base + x) `div` n] ++ a ;
      rem = (r*base + x) `mod` n ;
      (a,b) = (divSimple xs n base rem)
    }

prestamoDiv::Int -> Int -> Int
prestamoDiv x y
    | x < y = 1
    | otherwise = 0

-- recibe listas al reves
divaux::[Int] -> [Int] -> Int -> Int -> Int -> ([Int],[Int])
divaux [] (y:ys) base n d = ([],[])
divaux (x:xs) (y:ys) base n d =
    if (reverse prueba) > (reverse (x:xs))
    then divaux (x:xs) (y:ys) base (n-1) d
    else ([n], resta)
        where {
          resta = sustraccionEnt (x:xs) prueba base (prestamoDiv x (head prueba)) ;
          prueba = multiNumEnt (y:ys) n base 0
        }

-- recibe listas al derecho
dividir::[Int] -> [Int] -> Int -> Int -> Int -> [Int]
dividir [] (y:ys) base t d = []
dividir (x:xs) (y:ys) base t d =
    if x1 < (y:ys)
    then dividir (x:xs) (y:ys) base (t+1) d
    else 
        if x2 == []
        then resultado
        else resultado ++ (dividir (modulo ++ x2) (y:ys) base (base-1) d)
            where {
              (x1,x2) = splitAt t (x:xs) ;
              (resultado,modulo) = divaux (reverse (x:xs)) (reverse (y:ys)) base (base-1) d
            }
