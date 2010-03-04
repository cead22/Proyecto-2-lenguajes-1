data RealArbitrario = NoNeg [Int] [Int] Int
     		    | Neg [Int] [Int] Int
		      deriving (Eq)

------------------ convertir ----------------------------------
-- Transforma un RealArbitrario en su opuesto (Mismo numero
-- con diferente signo)
--
-- @param RealArbitrario : RealArbitrario al cual se
-- le desea cambiar el signo.
--
-- Retorna: RealArbitrario con signo opuesto
----------------------------------------------------------------
convertir::RealArbitrario -> RealArbitrario
convertir (NoNeg x y base) = Neg x y base
convertir (Neg x y base) = NoNeg x y base

----------------- compararBase -----------------------------------
-- Determina si dos bases son iguales.
--
-- @param Int: base1
-- @param Int base 2
--
--Retorna : True si las bases son iguales False en caso contario.
-----------------------------------------------------------------
compararBase::Int -> Int -> Bool
compararBase base1 base2 = 
	if base1 == base2 then True else False

-------------------- showRA --------------------------
-- Retorna un string que muestra un RealArbitrario en base 10.
--
-- @param RealArbitrario : parametro a transformar
--
-- Retorna : String que representa el RealArbitrario.
----------------------------------------------------------
showRA::RealArbitrario -> String
showRA (NoNeg x y base) = show ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1))
showRA (Neg x y base) = show (negate ((fromIntegral (parteEntera x base 0) :: Double) + (parteFraccionaria y base 1)))

-------------------- acarreoSum --------------------------
-- Determina si hay o no acarreo en la suma y retorna su valor.
--
-- @param Int : Numero que permite determina si hay que llevar
-- acarreo o no.
-- @param Int : Base en la que se encuentra el Int dado.
--
-- Retorna : Acarreo resultante
-----------------------------------------------------------
acarreoSum::Int -> Int -> Int
acarreoSum s base 
    | s >= base = 1
    | otherwise = 0

------------------ adicionEnt ----------------------------------
-- Calcula la suma de dos listas de enteros de la misma base
--
-- @param [Int] : primera lista a sumar (en orden inverso)
-- @param [Int] : segunda lista a sumar (en orden inverso)
-- @param Int : base de las listas.
-- @param Int : acarreo que lleva la suma.
--
-- Retorna: Lista con el resultado de la suma (en orden inverso)
-----------------------------------------------------------------
adicionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
adicionEnt [] [] base ac = [ac]
adicionEnt [] (x:xs) base ac = adicionEnt (x:xs) [ac] base 0
adicionEnt (x:xs) [] base ac = adicionEnt (x:xs) [ac] base 0
adicionEnt (x:xs) (y:ys) base ac = 
    [(x + y + ac) `mod` base] ++ (adicionEnt xs ys base c)
	where c = acarreoSum (x + ac + y) base

----------------------- sumaRA ----------------------------------
-- Calcula la suma de dos reales arbitrarios
--
-- @param RealArbitrario : primer parametro sumar
-- @param RealArbitrario : segundo parametro a sumar
--
-- Retorna: RealArbitrario resultante de la suma.
-----------------------------------------------------------------
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

----------------------- limpia ----------------------------------
-- Quita todos los 0s no significativos de un numero con representacion
-- de lista.
--
-- @param [Int] : Representacion del numero a tratar
--
-- Retorna: Lista equivanlente a la pasada como parametro pero sin
-- los ceros no significativos
-----------------------------------------------------------------
limpia::[Int] -> [Int]
limpia [] = []
limpia (x:xs) = if x == 0
                then limpia xs
                else (x:xs)

----------------------- limpiar ----------------------------------
-- Quita todos los 0s no significativos de un RealAribtrario
--
-- @param RealArbitrario : Representacion del numero a tratar
--
-- Retorna: Lista equivanlente a la pasada como parametro pero sin
-- los ceros no significativos
-----------------------------------------------------------------         
limpiar::RealArbitrario -> RealArbitrario
limpiar (NoNeg x y base) = 
    NoNeg (reverse (limpia (reverse x))) (reverse (limpia (reverse y))) base


-------------------mayorEstrictoporNumero -----------------------
-- Toma dos listas de igual tamano y determina si la primera
-- representa un numero mayor que el representado por la
-- segunda lista.
--
-- @param [Int] : Primera lista a comparar (en Orden)
-- @param [Int] : Segunda lista a comparar (en Orden)
--
-- Retorna: True si se cumple la condicion False en caso contrario
-----------------------------------------------------------------
mayorEstrictoPorNumero::[Int] -> [Int] -> Bool
mayorEstrictoPorNumero [] [] = False
mayorEstrictoPorNumero (x:xs) [] = True
mayorEstrictoPorNumero [] (y:ys) = False
mayorEstrictoPorNumero (x:xs) (y:ys)
    | x > y = True
    | x < y = False
    | otherwise = mayorEstrictoPorNumero xs ys

-------------------mayorEstrictoporNumero -----------------------
-- Toma dos listas en orden y sin 0s no significativos.Y determina
-- si la primera representa un numero mayor que el representado
-- por la segunda lista.
--
-- @param [Int] : Primera lista a comparar (en Orden)
-- @param [Int] : Segunda lista a comparar (en Orden)
--
-- Retorna: True si se cumple la condicion False en caso contrario
-----------------------------------------------------------------
mayorEstricto::[Int] -> [Int] -> Bool
mayorEstricto x y
    | length x > length y = True
    | length x == length y = mayorEstrictoPorNumero x y
    | otherwise = False

---------------------- mayor ------------------------------------
-- Toma dos RealArbitrario y determina si el primero es mayor
-- que el segundo
--
-- @param RealArbitrario : Primera parametro a comparar
-- @param RealArbitrario : Segunda parametro lista a comparar
--
-- Retorna: True si se cumple la condicion False en caso contrario
-----------------------------------------------------------------
mayor::RealArbitrario -> RealArbitrario -> Bool
mayor (NoNeg x1 y1 base1) (NoNeg x2 y2 base2)
    | mayorEstricto (reverse x1) (reverse x2) = True
    | reverse x1 == reverse x2 &&  y1 >= y2 = True
    | otherwise = False

-------------------- prestamo --------------------------
-- Determina si hay o no acarreo en la resta y retorna su valor.
--
-- @param Int : Numero que permite determina si hay que llevar
-- acarreo o no.
--
-- Retorna : Acarreo resultante
-----------------------------------------------------------
prestamo::Int -> Int
prestamo s
    | s < 0 = 1
    | otherwise = 0

------------------ sustracionEnt --------------------------------
-- Calcula la resta de dos listas de enteros de la misma base
--
-- @param [Int] : primera lista (en orden inverso)
-- @param [Int] : segunda lista  (en orden inverso)
-- @param Int : base de las listas.
-- @param Int : acarreo que lleva la resta.
--
-- Retorna: Lista con el resultado de la resta (en orden)
-----------------------------------------------------------------
sustraccionEnt::[Int] -> [Int] -> Int -> Int -> [Int]
sustraccionEnt [] [] base pr = []
sustraccionEnt [] (x:xs) base pr = [x - pr] ++ xs
sustraccionEnt (x:xs) [] base pr = [x - pr] ++ xs
sustraccionEnt (x:xs) (y:ys) base pr = 
    [(x - y - pr) `mod` base] ++ (sustraccionEnt xs ys base c)
	where c = prestamo (x - y - pr)

----------------------- restaRA ----------------------------------
-- Calcula la suma de dos reales arbitrarios. El primer paramatro
-- debe ser mayor que el segundo.
--
-- @param RealArbitrario : primer parametro restar
-- @param RealArbitrario : segundo parametro a restar
--
-- Retorna: RealArbitrario resultante de la resta
-----------------------------------------------------------------
restaR::RealArbitrario -> RealArbitrario -> RealArbitrario
restaR (NoNeg x1 y1 base1) (NoNeg x2 y2 base2) = 
    if compararBase base1 base2
    then  NoNeg y (reverse x) base1
    else  error "Error en las bases"    
<<<<<<< HEAD:p.hs
     where {
          (x,y) = splitAt t (sustraccionEnt ((reverse frac_1) ++ x1) ((reverse frac_2) ++ x2) base1 0);
          t = (max (length y1) (length y2)) ;
          frac_1 = y1 ++ [ 0 | j <- [1..(tam_2 - tam_1)] ] ;
          frac_2 = y2 ++ [ 0 | j <- [1..(tam_1 - tam_2)] ] ;
          tam_1 = length y1 ;
          tam_2 = length y2
        }
----------------------- restaRA ----------------------------------
-- Calcula la suma de dos reales arbitrarios, en caso de que el
-- primer argumento sea menor que el segundo calcula la resta 
-- opuesta (Resultado equivalente).
--
-- @param RealArbitrario : primer parametro restar
-- @param RealArbitrario : segundo parametro a restar
--
-- Retorna: RealArbitrario resultante de la resta
-----------------------------------------------------------------

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


-------------------- acarreoMult--------------------------
-- Determina si hay o no acarreo en la muitiplicacion y retorna
-- su valor
--
-- @param Int : Numero que permite determina si hay que llevar
-- acarreo o no.
-- @param Int : Base en la que se encuentra el Int dado.
--
-- Retorna : Acarreo resultante
-----------------------------------------------------------
acarreoMult::Int -> Int -> Int
acarreoMult s base
    | s >= base = s `div` base
    | otherwise = 0

------------------ multiNumEnt --------------------------------
-- Calcula la multiplicacion de un numero representado en una lista
-- con un numero entero 
--
-- @param [Int] : lista a multiplicar (en orden)
-- @param Int : entero por el cual se desea multiplicar
-- @param Int : base de los numero.
-- @param Int : acarreo que lleva la multiplicacion.
--
-- Retorna: Lista con el resultado de la multiplicacion(en orden)
-----------------------------------------------------------------
multiNumEnt::[Int] -> Int -> Int -> Int -> [Int]
multiNumEnt [] n base ac = [ac]
multiNumEnt (x:xs) n base ac = 
    [(x * n + ac) `mod` base] ++ (multiNumEnt xs n base c)
    where c = acarreoMult (x * n + ac) base

------------------ multiNumEnt --------------------------------
-- Calcula la multiplicacion de dos numero con representacion en listas 
--
-- @param [Int] : lista a multiplicar (en orden)
-- @param [Int] : lista por la que se multiplica multiplicar (en orden)
-- @param Int : base de los numero.
-- @param Int : acarreo que lleva la multiplicacion.
--
-- Retorna: Lista con el resultado de la multiplicacion(en orden inverso)
-----------------------------------------------------------------
multi::[Int] -> [Int] -> Int -> Int -> [Int]
multi x [] base c = []
multi x (y:ys) base c =
    adicionEnt ([ 0 | i <- [1..c]] ++ a) b base 0
    where {
      a = multiNumEnt (reverse x) (last (y:ys)) base 0 ;
      b = multi x (init (y:ys)) base (c + 1)
    }

----------------------- multRA ----------------------------------
-- Calcula la multiplicacion de dos reales arbitrarios.
--
-- @param RealArbitrario : primer parametro de la mutiplicacion
-- @param RealArbitrario : segundo parametro de la multiplicacion
--
-- Retorna: RealArbitrario resultante de la multiplicacion
-----------------------------------------------------------------
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

------------------ divSimple --------------------------------
-- Calcula la division de un numero representado en una lista
-- con un numero entero 
--
-- @param [Int] : lista a dividir (en orden)
-- @param Int : divisor
-- @param Int : base de los numero.
-- @param Int : resto.
--
-- Retorna: Lista con el resultado de la division(en orden)
-----------------------------------------------------------------
divSimple::[Int] -> Int -> Int -> Int -> ([Int],Int)
divSimple [] n base r = ([],r)

divSimple (x:xs) n base r =
    (q,b)
    where {
      q = [(r*base + x) `div` n] ++ a ;
      rem = (r*base + x) `mod` n ;
      (a,b) = (divSimple xs n base rem)
    }
-------------------- prestamoDiv--------------------------
-- Determina si hay o no prestamo en la division y retorna
-- su valor
--
-- @param Int : Numero que permite determina si hay que pedir
--prestamo
-- @param Int : Base en la que se encuentra el Int dado.
--
-- Retorna : prestamo resultante.
-----------------------------------------------------------
prestamoDiv::Int -> Int -> Int
prestamoDiv x y
    | x < y = 1
    | otherwise = 0

------------------------ divaux --------------------------------
-- Calcula la division de dos numero con representacion en listas 
--
-- @param [Int] : lista dividendo (en orden inverso)
-- @param [Int] : lista divisor (en orden inverso)
-- @param Int : base de los numero.
-- @param Int : prestamo
--
-- Retorna: Lista con el resultado de la division (en orden inverso)
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

---------------------------- div1 --------------------------------
-- Calcula la division de dos numero con representacion en listas 
--
-- @param [Int] : lista dividendo (en orden inverso)
-- @param [Int] : lista divisor (en orden inverso)
-- @param Int : base de los numero.
-- @param Int : prestamo
-- @parame [Int] : resultado de la division
--
-- Retorna: Lista con el resultado de la division (en orden) y una lista
-- con el modulo de la division. 
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

---------------------------- div2 --------------------------------
-- Calcula la division de dos numero con representacion en listas,
-- anadiendo 0 al resultado cuando sea necesario. 
--
-- @param [Int] : lista dividendo (en orden inverso)
-- @param [Int] : lista divisor (en orden inverso)
-- @param Int : base de los numero.
-- @param Int : prestamo
-- @parame [Int] : resultado de la division
--
-- Retorna: Lista con el resultado de la division (en orden) y una lista
-- con el modulo de la division. 
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

----------------------- divRA ----------------------------------
-- Calcula la division de dos reales arbitrarios.
--
-- @param RealArbitrario : divisor
-- @param RealArbitrario : dividendo
--
-- Retorna: RealArbitrario resultante de la division
-----------------------------------------------------------------
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
