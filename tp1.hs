import System.IO
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos x = verificarCubos x 1 (truncate (fromIntegral x**(1/3))) 

verificarCubos x a b    | n == x = True
                        | a > b = False
                        | n < x = verificarCubos x (a+1) b
                        | n > x = verificarCubos x a (b-1)
                        | otherwise = False
                            where n = a^3 + b^3


-- Ejercicio 2 --------------------------------------------------------------

descomposicionCubos :: Integer -> (Integer, Integer)
descomposicionCubos x = buscarCubos x 1 (truncate (fromIntegral x**(1/3)))

buscarCubos x a b   | n == x = (a, b)
                    | a > b = (0,0) -- Si no encuentra devuelve (0,0)
                    | n < x = buscarCubos x (a+1) b
                    | n > x = buscarCubos x a (b-1)
                    | otherwise = (0,0)
                        where n = a^3 + b^3                    

-- Ejercicio 3 ---------------------------------------------------------------
cantidadDeFormas :: Integer -> Integer
cantidadDeFormas x = buscarCantidad x 1 (truncate (fromIntegral x**(1/3)))
buscarCantidad x a b    | a > b = 0
                        | n == x = 1 + buscarCantidad x (a+1) (b-1)
                        | n < x = buscarCantidad x (a+1) b
                        | n > x = buscarCantidad x a (b-1)
                        | otherwise = 0
                            where n = a^3 + b^3
                            
----------------------FUNCIONES UTILES-----------------------------

esEspecial :: Integer -> Bool -- Esta funcion es para saber si un numero es especial y asi no ocupar espacio en las otras funciones
esEspecial x = cantidadDeFormas x > 1

-------------------------------------------------------------------


-- Ejercicio 4 ----------------------------------------------------------------

especialDesde :: Integer -> Integer
especialDesde x | esEspecial x = x -- Llamo a la funcion "util" por comodidad de organización.
                | otherwise = especialDesde (x+1)
                
-- Ejercicio 5 -----------------------------------------------------------------

especialNumero :: Integer -> Integer
especialNumero n_esimo_especial = contadorEspecial n_esimo_especial 1 1730

contadorEspecial :: Integer -> Integer -> Integer -> Integer
contadorEspecial n_esimo_especial contador n  | 0 == n_esimo_especial = 0
                        | contador == n_esimo_especial = n - 1 
                        | esEspecial n = contadorEspecial n_esimo_especial (contador + 1) (n + 1)
                        | otherwise = contadorEspecial n_esimo_especial contador (n + 1)

-- Ejercicio 6 -----------------------------------------------------------------

esMuyEspecial :: Integer -> Bool
esMuyEspecial x = esEspecial x && condicionSegunda 1 x 2 1

condicionSegunda :: Integer -> Integer -> Integer -> Integer -> Bool
condicionSegunda n x a b | b == div x (a^3) && ((esEspecial b)==True) = False
                         | a^3 > x = False
                         | b > div x a^3 = False
                         | b /= div x a^3 && ((esEspecial b)==True) = True
                         | n < x = condicionSegunda n x (a) (b+1)
                         | n > x = condicionSegunda n x (a+1) (0)
                         | otherwise = True
