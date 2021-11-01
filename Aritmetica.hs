module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits
import Data.List

--(1)
-- Ignorar esto
mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt _ _ = (0, (0, 0))


-- Every prime number can be represented in form of 6n+1 or 6n-1 except the prime number 2 and 3, where n is a natural number.
--(2)-----------------------------------------------------------------------------
criba :: Integer -> Set Integer
criba num = encontrarPrimos num 1 [2, 3]

encontrarPrimos :: Integer -> Integer -> Set Integer -> Set Integer
encontrarPrimos num it set  | num == 1 = []
                            | num == 2 = [2]
                            | num == 3 = set
                            | a > num = set
                            | b < num && esPrimo a && esPrimo b = encontrarPrimos num (it+1) ((set ++ [a])++[b])
                            | esPrimo a = encontrarPrimos num (it+1) (set ++ [a])
                            | b > num = set
                            | esPrimo b = encontrarPrimos num (it+1) (set ++ [b])
                            | otherwise = encontrarPrimos num (it+1) set
                            where
                              a = 6*it-1
                              b = 6*it+1

esPrimo :: Integral a => a -> Bool
esPrimo n = verificarEsPrimo (fromIntegral n) 2

verificarEsPrimo :: Integral a => a -> a -> Bool
verificarEsPrimo n i  | n == 1 = False
                      | n == i = True
                      | n `mod` i == 0 = False
                      | i > fromIntegral sqrtn = True
                      | otherwise = verificarEsPrimo n (i+1)
                      where sqrtn = round (sqrt (fromIntegral n))

-- criba de 10,000 sin i > sqrtn tarda 17.14 segundos :-/ me gustaria que tarde 5 como mucho
-- con i > sqrtn tarda 1.3 segundos aprox :-D ahi anda bien

--(3)-----------------------------------------------------------------------------
--Example 1. Let the two numbers be 10 and 90.
-- We have,
-- 45 = 3×3×5 
-- 90 = 2×3×3×5
-- Common factors = 3,3 and 5
-- Therefore, gcd(45,90)=3×3×5=45≠1⇒45 and 90 are not coprime numbers.

-- Entoces para sacar coprimos de n buscamos primos que no sean factor comun a n
-- Para eso hago una funcion que sea sacar factores comunes

coprimoCon:: Integer -> Integer
coprimoCon num = encontrarCoprimo num (nub(factores num)) 2 0

factores num = verificarFactores num 2 []

verificarFactores num it set | num == 1 = set
                             | esPrimo num = set++[num]
                             | num `mod` it == 0 = verificarFactores (num `div` it) 2 (set ++ [it])
                             | otherwise = verificarFactores num (it+1) set

encontrarCoprimo num set i it | num == 2 = 1 -- coprimo con 2
                              | it >= length set = i
                              | i `mod` (set !! it) /= 0 = encontrarCoprimo num set i (it+1)
                              | otherwise = encontrarCoprimo num set (i+1) 0

--(4)
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo num1 num2 | not (sonCoprimos num1 num2) = -99
                                | otherwise = euclidesExtendido num2 num1 1 0 0 1 num2

-- Verifico si son coprimos
sonCoprimos num1 num2 = boolFactoresDistintos (factores num1) (factores num2) 0 0

boolFactoresDistintos set1 set2 it1 it2 | (set1 !! it1) == (set2 !! it2) = False
                                        | it1 < length set1 -1 = boolFactoresDistintos set1 set2 (it1+1) it2
                                        | it2 < length set2 -1 = boolFactoresDistintos set1 set2 0 (it2+1)
                                        | otherwise = True

-- Aplico el algoritmo de Euclides Extendido
-- https://www.youtube.com/watch?v=D289EF58Yrw
euclidesExtendido :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
euclidesExtendido g0 g1 u0 u1 v0 v1 num2 | g1 /= 0 = euclidesExtendido g1 g2 u1 u2 v1 v2 num2
                                       --                              g0 g1 u0 u1 v0 v1 B queda igual
                                      | g1 == 0 && v0 < 0 = v0 + num2
                                      | otherwise = v0
                                      where 
                                        y2 = g0 `div` g1
                                        g2 = g0 - y2 * g1
                                        u2 = u0 - y2 * u1
                                        v2 = v0 - y2 * v1

-- Metete la función de regalo por el *rt*

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

-- inv(9, 275)
