module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits
import Data.List

--(1)-----------------------------------------------------------------------------

-- No obliga a hacer el algortmo criba así que hice uno mejor
criba :: Integer -> Set Integer
criba num = encontrarPrimos num 1 [2, 3]

encontrarPrimos :: Integer -> Integer -> Set Integer -> Set Integer
encontrarPrimos num it set  | num == 1 || num == 2 = [] -- no hay primos anteriores a 2
                            | num == 3 = [2] -- solo 2 ya que cuentan los anteriores a 3
                            | a > num = set -- ya me fui del num
                            | b < num && esPrimo a && esPrimo b = encontrarPrimos num (it+1) ((set ++ [a])++[b])  --es primo a y b con b es menor que num
                            | esPrimo a = encontrarPrimos num (it+1) (set ++ [a]) -- es solo primo a
                            | b > num = set -- ya me fui del num 
                            | esPrimo b = encontrarPrimos num (it+1) (set ++ [b]) -- es solo primo b
                            | otherwise = encontrarPrimos num (it+1) set --proximos a y b
                            where
                              a = 6*it-1
                              b = 6*it+1

esPrimo :: Integral a => a -> Bool
esPrimo n = verificarEsPrimo (fromIntegral n) 2

verificarEsPrimo :: Integral a => a -> a -> Bool
verificarEsPrimo n i  | n == 1 = False -- 1 no es primo
                      | n == 3 = True -- 3 es Primo
                      | n `mod` i == 0 = False -- es compuesto
                      | i > fromIntegral sqrtn = True -- verificar más números no sirve
                      | otherwise = verificarEsPrimo n (i+1) -- proximo i
                      where sqrtn = round (sqrt (fromIntegral n))

-- criba de 10,000 sin i > sqrtn tarda 17.14 segundos :-/ me gustaria que tarde 5 como mucho
-- con i > sqrtn tarda 1.3 segundos aprox :-D ahi anda bien

--(2)-----------------------------------------------------------------------------

-- Entoces para sacar coprimos de n buscamos primos que no sean factor comun a n
-- Para eso hago una funcion que sea sacar factores comunes

coprimoCon:: Integer -> Integer
coprimoCon num = encontrarCoprimo num (nub(factores num)) 2 0 --nub saca duplicados

factores num = verificarFactores num 2 []

verificarFactores num it set | num == 1 = set --llegue al final
                             | esPrimo num = set++[num] -- si lo que queda es primo agrego al set y lo devuelvo
                             | num `mod` it == 0 = verificarFactores (num `div` it) it (set ++ [it]) --encontre un factor :)
                             | otherwise = verificarFactores num (it+1) set -- proximo it

encontrarCoprimo num set i it | num == 2 = 1 -- coprimo con 2 
                              | it >= length set = i -- si no es compuesto con otro, llega al final de la lista
                              | i `mod` (set !! it) /= 0 = encontrarCoprimo num set i (it+1) -- si no es compuesto con uno verifico con el resto del set
                              | otherwise = encontrarCoprimo num set (i+1) 0 -- proximo i

--()
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo num1 num2 | not (sonCoprimos num1 num2) = -99
                                | a < 0 = a + num2
                                | a > 0 = a
                                  where a = euclidesExtendido num2 num1 1 0 0 1

-- Verifico si son coprimos
sonCoprimos num1 num2 = boolFactoresDistintos (factores num1) (factores num2) 0 0

boolFactoresDistintos set1 set2 it1 it2 | (set1 !! it1) == (set2 !! it2) = False -- tienen factor común
                                        | it1 < length set1 -1 = boolFactoresDistintos set1 set2 (it1+1) it2 --proximo elemento en set1
                                        | it2 < length set2 -1 = boolFactoresDistintos set1 set2 0 (it2+1) --proximo elemento en set2
                                        | otherwise = True --llegue al final :=)

-- Aplico el algoritmo de Euclides Extendido
-- https://www.youtube.com/watch?v=D289EF58Yrw
euclidesExtendido :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
euclidesExtendido g0 g1 u0 u1 v0 v1 | g1 /= 0 = euclidesExtendido g1 g2 u1 u2 v1 v2
                                       --                              g0 g1 u0 u1 v0 v1 queda igual
                                    | g1 == 0 = v0 
                                      where 
                                        y2 = g0 `div` g1 --mirar el video
                                        g2 = g0 - y2 * g1
                                        u2 = u0 - y2 * u1
                                        v2 = v0 - y2 * v1

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer -- si saben que hace diganme x que no se para que sirve
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

