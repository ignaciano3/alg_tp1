-- To find all the pairs of integers x and y that sum to n when cubed,
-- set x to the largest integer less than the cube root of n, 
-- set y to 0, then repeatedly add 1 to y if the sum of the cubes is less than n,
-- subtract 1 from x if the sum of the cubes is greater than n,
-- and output the pair otherwise, stopping when x and y cross. 
-- If you only want to know whether or not such a pair exists, you can stop as soon as you find one.

-- Ejercicio 1 ---------------------------------------------------------------

esSumaDeDosCubos :: Integer -> Bool

-- cota maxima = sqrt3(x) = n -> truncada
-- cota minima = sqrt3(x - n^3) -> truncada
-- ej:
-- x = 108736
-- n = 47 = sqrt3(108736)
-- min =  17 = sqrt3(108736 - 43^3)

esSumaDeDosCubos x = verificarCubos x
                        (truncate ((fromIntegral x - fromIntegral n^3) ** (1/3)))
                        n
                        where n = truncate (fromIntegral x**(1/3))

verificarCubos x a b    | n == x = True
                        | a > b = False
                        | n < x = verificarCubos x (a+1) b
                        | n > x = verificarCubos x a (b-1)
                        | otherwise = False
                            where n = a^3 + b^3

-- Ejercicio 2 --------------------------------------------------------------

descomposicionCubos :: Integer -> (Integer, Integer)
descomposicionCubos x = buscarCubos x
                         (truncate ((fromIntegral x - fromIntegral n^3) ** (1/3))) 
                         n
                         where n = truncate (fromIntegral x**(1/3))

buscarCubos x a b   | n == x = (a, b)
                    | a > b = (0,0) -- Si no encuentra devuelve (0,0)
                    | n < x = buscarCubos x (a+1) b
                    | n > x = buscarCubos x a (b-1)
                    | otherwise = (0,0)
                        where n = a^3 + b^3

-- Ejercicio 3 ---------------------------------------------------------------
cantidadDeFormas :: Integer -> Integer
cantidadDeFormas x = buscarCantidad x
                        (truncate ((fromIntegral x - fromIntegral n^3) ** (1/3)))
                        n
                        where n = truncate (fromIntegral x**(1/3))

buscarCantidad x a b    | a > b = 0
                        | n == x = 1 + buscarCantidad x (a+1) b
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
especialNumero x = contadorEspecial x 0 0

contadorEspecial :: Integer -> Integer -> Integer -> Integer
contadorEspecial x k n  | k == x = n - 1 -- Resto uno porque hace una vuelta de más.
                        | esEspecial n = contadorEspecial x (k + 1) (n + 1)
                        | otherwise = contadorEspecial x k (n + 1)

-- Ejercicio 6 -----------------------------------------------------------------

esMuyEspecial :: Integer -> Bool
esMuyEspecial x = esEspecial x && condicionSegunda x 2 1

condicionSegunda :: Integer -> Integer -> Integer -> Bool
condicionSegunda x a b | n == x = False
                       | b == div n 8 = True -- Funciona como techo de la funcion.
                       | n < x = condicionSegunda x (a+1) b
                       | n > x = condicionSegunda x 2 (b + 1)
                            where n = a^3 * b
