-- Ejercicio 1
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos x = True 

-- Ejercicio 2
-- x = a^3+b^3
-- sol = (a, sqrt3(x-b^3))

a :: Integer
a = 2

b :: Integer
b = 1

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