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

sumaRA (NoNeg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = 
    restaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2)

sumaRA (Neg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2) = 
    restaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2)

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
mayorEstrictoPorNumero (x:xs) [] = True
mayorEstrictoPorNumero [] (y:ys) = False
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

mayorOIgualPorNumero::[Int] -> [Int] -> Bool
mayorOIgualPorNumero [] [] = True
mayorOIgualPorNumero (x:xs) (y:ys)
   | x > y = True
   | x < y = False
   | otherwise = mayorOIgualPorNumero xs ys

mayorOIgual::[Int] -> [Int] -> Bool
mayorOIgual x y
   | length x > length y = True
   | length x == length y = mayorOIgualPorNumero x y
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
    | reverse x1 == reverse x2 &&  y1 >= y2 = True
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
    if mayorOIgual y1 y2 
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

restaRA (NoNeg x1 y1 base1) (Neg x2 y2 base2) = sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
restaRA (Neg x1 y1 base1) (NoNeg x2 y2 base2) = convertir (sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))

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

multRA (Neg x1 y1 base1) (Neg x2 y2 base2) = multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
multRA (NoNeg x1 y1 base1) (Neg x2 y2 base2) = convertir (multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))
multRA (Neg x1 y1 base1) (NoNeg x2 y2 base2) = convertir (multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))

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
divaux::[Int] -> [Int] -> Int -> Int -> ([Int],[Int])
divaux [] (y:ys) base n = ([],[0])
divaux (x:xs) (y:ys) base n =
    if sum (x:xs) == 0
    then ([0],[0])
    else
        if mayorEstricto (reverse prueba) (reverse (x:xs))
        then divaux (x:xs) (y:ys) base (n-1)
        else ([n], limpia (reverse resta))
            where {
              resta = sustraccionEnt (x:xs) prueba base 0 ;--(prestamoDiv x (head  (prueba))) ;
              prueba = reverse(limpia(reverse(multiNumEnt (y:ys) n base 0)))
            }


div1::[Int] -> [Int] -> Int -> Int -> [Int] -> ([Int],[Int])
div1 [] (y:ys) base i res = (res,[0])
div1 [0] (y:ys) base i res = (res++[0],[0])
div1 (x:xs) (y:ys) base i res =
   if mayorEstricto (limpia (y:ys)) (limpia x1) -- x1 < y:ys 
   then if x2 == []
        then (res++resultado,modulo)
        else div1 (x:xs) (y:ys) base (i+1) res --2
   else   
       if x2 == [] 
       then (res++resultado,modulo)
       else (div2 (modulo ++ x2) (y:ys) base (length modulo) (res++resultado)) --2
           where {
             (x1,x2) = splitAt (i+1) (x:xs) ;
             (resultado,modulo) = divaux (reverse (x1)) (reverse (y:ys)) base (base-1)
           }

div2::[Int] -> [Int]-> Int -> Int -> [Int] -> ([Int],[Int])
div2 [] (y:ys) base i res = (res,[0])
div2 [0] (y:ys) base i res = (res++[0],[0])
div2 (x:xs) (y:ys) base i res =
    if mayorEstricto (limpia (y:ys)) (limpia x1) -- x1 < y:ys 
    then 
        if x2 == []
        then (res++resultado,modulo)
        else div2 (x:xs) (y:ys) base (i+1) (res++[0])
    else   
        if x2 == [] 
        then (res++resultado,modulo)
        else div2 (modulo ++ x2) (y:ys) base (length modulo) (res++resultado)
    where {
      (x1,x2) = splitAt (i+1) (x:xs) ;
      (resultado,modulo) = divaux (reverse (x1)) (reverse (y:ys)) base (base-1)
    }

divConDecimales::[Int] -> [Int] -> Int -> Int -> ([Int],[Int])
divConDecimales (x:xs) (y:ys) base decimales =
    (ent,frac)
    where {
      (ent,modent) = div1 (x:xs) (y:ys) base 0 [];
      (frac,modfrac) = div1 (modent ++ [ 0 | j <- [1..decimales]]) (y:ys) base 0 []
    }

divRA::RealArbitrario -> RealArbitrario -> Int -> RealArbitrario
divRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) decimales =
    if (length y1 >= length y2)
    then (NoNeg (reverse ent1) frac1 base1)
    else (NoNeg (reverse ent2) frac2 base1)
    where {
      (ent1,frac1) = divConDecimales ((reverse x1) ++ y1) ((reverse x2) ++ y2 ++ ceros1) base1 decimales ;
      ceros1 = [ 0 | j <- [1..((length y1) - (length y2))] ] ;

      (ent2,frac2) = divConDecimales ((reverse x1) ++ y1 ++ ceros2) ((reverse x2) ++ y2) base1 decimales ;
      ceros2 = [ 0 | j <- [1..((length y2) - (length y1))] ]
    }
       
elevadoALa::[Int] -> [Int] -> Int -> Int -> [Int]
elevadoALa x y base 1 = y
elevadoALa x y base n =
    elevadoALa x (limpia (reverse (multi x y base 0))) base (n-1)


entALista::Int -> [Int] -> [Int]
entALista 0 y = y
entALista x y = entALista (x `div` 10) y ++ [(x `mod` 10)]

piAux::[Int] -> Int -> RealArbitrario
--piAux [] decimales = NoNeg [3] ([1]++[ 3 | j <- [1..decimales] ]) 10
piAux (n:ns) decimales =
    if all (\x -> x == 0) (n:ns)
    then NoNeg [3] ([1]++[ 3 | j <- [1..(decimales-1)] ]) 10
    else sumaRA termino_n (piAux (sustraccionEnt (n:ns) [1] 10 0) decimales)
    where {
      termino_n =  multRA primer_factor segundo_factor ;
      primer_factor = divRA (NoNeg [1] [] 10) dieciseis_i decimales ;
      dieciseis_i = NoNeg (elevadoALa [16] [16] 10 (mostrarEnt (reverse (n:ns)) 10)) [] 10 ;
      segundo_factor = restaRA s1 (sumaRA s2 (sumaRA s3 s4)) ;
      s1 = divRA (NoNeg [4] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [1] [] 10)) decimales ;
      s2 = divRA (NoNeg [2] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [4] [] 10)) decimales ;
      s3 = divRA (NoNeg [1] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [5] [] 10)) decimales ;
      s4 = divRA (NoNeg [1] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [6] [] 10)) decimales
    }


mostrarEnt::[Int] -> Int -> Int
mostrarEnt (x:xs) base
    | (length (x:xs)) == 1 = x
    | otherwise =  (x + base * (mostrarEnt xs base))

terminoPI::[Int] -> Int -> RealArbitrario
terminoPI (n:ns) decimales =
    if all (\x -> x == 0) (n:ns)
    then NoNeg [3] ([1]++[ 3 | j <- [1..(decimales-1)] ]) 10
    else termino_n
    where {
      termino_n = multRA primer_factor segundo_factor ;
      primer_factor = divRA (NoNeg [1] [] 10) dieciseis_i decimales ;
      dieciseis_i = NoNeg (elevadoALa [16] [16] 10 (mostrarEnt (reverse (n:ns)) 10)) [] 10 ;
      segundo_factor = restaRA s1 (sumaRA s2 (sumaRA s3 s4)) ;
      s1 = divRA (NoNeg [4] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [1] [] 10)) decimales ;
      s2 = divRA (NoNeg [2] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [4] [] 10)) decimales ;
      s3 = divRA (NoNeg [1] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [5] [] 10)) decimales ;
      s4 = divRA (NoNeg [1] [] 10) (sumaRA (multRA (NoNeg [8] [] 10) (NoNeg (n:ns) [] 10)) (NoNeg [6] [] 10)) decimales
    }