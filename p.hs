data RealArbitrario = NoNeg [Int] [Int] Int
     		    | Neg [Int] [Int] Int
		      deriving (Eq)

convertir::RealArbitrario -> RealArbitrario
convertir (NoNeg x y base) = Neg x y base
convertir (Neg x y base) = NoNeg x y base

compararBase::Int -> Int -> Bool
compararBase base1 base2 = 
	if base1 == base2 then True else False

acarreoSum::Int -> Int -> Int
acarreoSum s base 
    | s >= base = 1
    | otherwise = 0

adicionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
adicionEnt [] [] base ac = [ac]
adicionEnt [] (x:xs) base ac = adicionEnt (x:xs) [ac] base 0
adicionEnt (x:xs) [] base ac = adicionEnt (x:xs) [ac] base 0
adicionEnt (x:xs) (y:ys) base ac = 
    [(x + y + ac) `mod` base] ++ (adicionEnt xs ys base c)
	where c = acarreoSum (x + ac + y) base

sumaRA::RealArbitrario -> RealArbitrario -> RealArbitrario

sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    if compararBase base1 base2
    then  NoNeg y (reverse x) base1
    else  error "Error en las bases"    
    where {
      (x,y) = splitAt t (adicionEnt ((reverse frac_1) ++ x1) ((reverse frac_2) ++ x2) base1 0);
      t = (max (length y1) (length y2)) ;
      frac_1 = y1 ++ [ 0 | j <- [1..(tam_2 - tam_1)] ] ;
      frac_2 = y2 ++ [ 0 | j <- [1..(tam_1 - tam_2)] ] ;
      tam_1 = length y1 ;
      tam_2 = length y2
    }

sumaRA (Neg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = 
    convertir (sumaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2))

sumaRA (NoNeg (x:xs) (y:ys) base1) (Neg (z:zs) (w:ws) base2) = 
    restaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2)

sumaRA (Neg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2) = 
    restaRA (NoNeg (x:xs) (y:ys) base1) (NoNeg (z:zs) (w:ws) base2)

limpia::[Int] -> [Int]
limpia [] = []
limpia (x:xs) = 
    if x == 0
    then limpia xs
    else (x:xs)
          
limpiar::RealArbitrario -> RealArbitrario
limpiar (NoNeg x y base) = 
    NoNeg (reverse (limpia (reverse x))) (reverse (limpia (reverse y))) base

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

restaR::RealArbitrario -> RealArbitrario -> RealArbitrario
restaR (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    if compararBase base1 base2
    then  NoNeg y (reverse x) base1
    else  error "Error en las bases"    
        where {
          (x,y) = splitAt t (sustraccionEnt ((reverse frac_1) ++ x1) ((reverse frac_2) ++ x2) base1 0);
          t = (max (length y1) (length y2)) ;
          frac_1 = y1 ++ [ 0 | j <- [1..(tam_2 - tam_1)] ] ;
          frac_2 = y2 ++ [ 0 | j <- [1..(tam_1 - tam_2)] ] ;
          tam_1 = length y1 ;
          tam_2 = length y2
        }

restaRA::RealArbitrario -> RealArbitrario -> RealArbitrario
restaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | (mayor num1 num2) = restaR num1 num2
    | otherwise = convertir (restaR num2 num1)
    where {
      num1 = limpiar (NoNeg x1 y1 base1) ;
      num2 = limpiar (NoNeg x2 y2 base2)
    }

restaRA (NoNeg x1 y1 base1) (Neg x2 y2 base2) =
    sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
restaRA (Neg x1 y1 base1) (NoNeg x2 y2 base2) = 
    convertir (sumaRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))



acarreoMult::Int -> Int -> Int
acarreoMult s base
    | s >= base = s `div` base
    | otherwise = 0

multiNumEnt::[Int] -> Int -> Int -> Int -> [Int]
multiNumEnt [] n base ac = [ac]
multiNumEnt (x:xs) n base ac = 
    [(x * n + ac) `mod` base] ++ (multiNumEnt xs n base c)
    where c = acarreoMult (x * n + ac) base

-- recibe listas al derecho
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
    if compararBase base1 base2
    then  NoNeg y (reverse x) base1
    else  error "Error en las bases" 
        where {
          (x,y) = splitAt ((length y1) + (length y2)) z ;
          z = multi ((reverse x1) ++ y1) ((reverse x2) ++ y2) base1 0
        }

multRA (Neg x1 y1 base1) (Neg x2 y2 base2) = 
    multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)

multRA (NoNeg x1 y1 base1) (Neg x2 y2 base2) =
    convertir (multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))

multRA (Neg x1 y1 base1) (NoNeg x2 y2 base2) =
    convertir (multRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2))

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
div1 (x:xs) [] base i res = (res,[0]) --flecha
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
div2 (x:xs) [] base i res = (res,[0]) --flecha
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
divConDecimales x y base decimales =
    (ent,frac)
    where {
      (ent,modent) = div1 x y base 0 [];
      (frac,modfrac) = div2 (modent ++ [ 0 | j <- [1..decimales]]) y base ((length modent))  []
    }

divRA::RealArbitrario -> RealArbitrario -> Int -> RealArbitrario
divRA (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) decimales =
    if compararBase base1 base2
    then if (sum x2) + (sum y2) == 0
	 then error "Division por Cero"
	 else if (length y1 >= length y2)
	     then (NoNeg (reverse ent1) frac1 base1)
	     else (NoNeg (reverse ent2) frac2 base1)
    	else  error "Error en las bases" 
    
    where {
      (ent1,frac1) = divConDecimales ((reverse x1) ++ y1) ((reverse x2) ++ y2 ++ ceros1) base1 decimales ;
      ceros1 = [ 0 | j <- [1..((length y1) - (length y2))] ] ;

      (ent2,frac2) = divConDecimales ((reverse x1) ++ y1 ++ ceros2) ((reverse x2) ++ y2) base1 decimales ;
      ceros2 = [ 0 | j <- [1..((length y2) - (length y1))] ]
    }
       
enDigitos::[Int] -> [Int]
enDigitos [] = []
enDigitos (x:xs)
    | x > 9 = [x `div` 10] ++ [x `mod` 10] ++ xs
    | otherwise = x:xs

elevadoALa::[Int] -> [Int] -> Int -> Int -> [Int]
elevadoALa x y base 1 = enDigitos y
elevadoALa x y base n =
    elevadoALa x (limpia (reverse (multi x y base 0))) base (n-1)

entALista::Int -> [Int] -> [Int]
entALista 0 y = y
entALista x y = [(x `mod` 10)] ++ entALista (x `div` 10) y

piRA::Int -> Int -> RealArbitrario
piRA n decimales =
    if n == 0
    then terminoPI 0 decimales
    else 
        sumaRA (terminoPI n decimales) (piRA (n-1) decimales)



terminoPI::Int -> Int -> RealArbitrario
terminoPI 0 decimales = NoNeg [3] ([1]++[ 3 | j <- [1..(decimales-1)] ]) 10
terminoPI n decimales =
    termino_n
    where {
      termino_n = multRA primer_factor segundo_factor ;
      primer_factor = limpiar (divRA (NoNeg [1] [] 10) dieciseis_i decimales) ;
      dieciseis_i = NoNeg (reverse (elevadoALa [1,6] [1,6] 10 n)) [] 10 ;
      segundo_factor = limpiar (restaRA s1 (sumaRA s2 (sumaRA s3 s4))) ;
      s1 = divRA real_cuatro (sumaRA (multRA real_ocho (NoNeg ent_l [] 10)) real_uno) decimales ;
      s2 = divRA real_dos (sumaRA (multRA real_ocho (NoNeg ent_l [] 10)) real_cuatro) decimales ;
      s3 = divRA real_uno (sumaRA (multRA real_ocho (NoNeg ent_l [] 10)) real_cinco) decimales ;
      s4 = divRA (NoNeg [1] [] 10) (sumaRA (multRA real_ocho (NoNeg (ent_l) [] 10)) real_seis) decimales ;
      ent_l = (entALista n []) ;
      real_ocho   = (NoNeg [8] [] 10) ;
      real_cuatro = (NoNeg [4] [] 10) ;
      real_cinco  = (NoNeg [5] [] 10) ;
      real_seis   = (NoNeg [6] [] 10) ;
      real_dos    = (NoNeg [2] [] 10) ;
      real_uno    = (NoNeg [1] [] 10)
    }

mostrarEnt::[Int] -> Int -> Int
mostrarEnt [] base = 0
mostrarEnt (x:xs) base
    | (length (x:xs)) == 1 = x
    | otherwise = x + base * (mostrarEnt xs base)

mostrarFrac::[Int] -> Int -> Double
mostrarFrac [] base = 0
mostrarFrac (x:xs) base = ((fromIntegral x :: Double) + (mostrarFrac xs base)) / (fromIntegral base :: Double)

mostrarLista:: [Int] -> String
mostrarLista [] = ""
mostrarLista (x:xs) = (show x) ++ (mostrarLista xs)

showAux::RealArbitrario -> String
showAux (NoNeg [] [] base) = "0"
showAux (NoNeg [] (y:ys) base) = "0." ++ (mostrarLista (y:ys))
showAux (NoNeg (x:xs) [] base) = mostrarLista (x:xs)
showAux (NoNeg (x:xs) (y:ys) base) =
    (mostrarLista x_limpia) ++ "." ++ (mostrarLista y_limpia)
    where {
      x_limpia = limpia (reverse (x:xs)) ;
      y_limpia = reverse (limpia (reverse (y:ys)))
    }

showDecimal::RealArbitrario -> String
showDecimal (NoNeg x y base) = showAux (NoNeg x y base)
showDecimal (Neg x y base) = "-" ++ showAux (NoNeg x y base)

showRA::RealArbitrario -> String
showRA (NoNeg x y 10) = showDecimal (limpiar (NoNeg x y 10))
showRA (Neg x y 10) = showDecimal (limpiar (Neg x y 10))
showRA (NoNeg x y base) = show ((fromIntegral (mostrarEnt x base) :: Double)  + (mostrarFrac y base))
showRA (Neg x y base) = show (negate ((fromIntegral (mostrarEnt x base) :: Double)  + (mostrarFrac y base)))


obtenerEnt::RealArbitrario -> [Int]
obtenerEnt (NoNeg x y base) = x
obtenerEnt (Neg x y base) = x

obtenerFrac::RealArbitrario -> [Int]
obtenerFrac (NoNeg x y base) = y
obtenerFrac (Neg x y base) = y
