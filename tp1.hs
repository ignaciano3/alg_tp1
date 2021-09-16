-- To find all the pairs of integers x and y that sum to n when cubed,
-- set x to the largest integer less than the cube root of n, 
-- set y to 0, then repeatedly add 1 to y if the sum of the cubes is less than n,
-- subtract 1 from x if the sum of the cubes is greater than n,
-- and output the pair otherwise, stopping when x and y cross. 
-- If you only want to know whether or not such a pair exists, you can stop as soon as you find one.



-- Ejercicio 1
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos x = True 

-- Ejercicio 2

a = 0

f x = truncate (fromIntegral x**(1/3)) 

b :: Integer
b = 1

-- No se como iterar el a y el b
descomposicionCubos :: Integer -> (Integer, Integer)
descomposicionCubos x | a^3 + b^3 == x = (a,b)
                      | a^3 > x = (a-1, b)
                      | b^3 > x = (a, b-1)
                      | otherwise = (0,0)
--descomposicionCubos x = (a, floor ((x - fromIntegral a ^ 3) ** (1/3))) 




-- Ejercicio 3
cantidadDeFormas :: Integer -> Integer
cantidadDeFormas x = 1

-- Ejercicio 4
especialDesde :: Integer -> Integer
especialDesde x = x

-- Ejercicio 5
especialNumero :: Integer -> Integer
especialNumero x = x

-- Ejercicio 6
esMuyEspecial :: Integer -> Bool
esMuyEspecial x = True