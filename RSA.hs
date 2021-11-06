module RSA where
import Tipos
import Aritmetica

--(4)
-- en esta funcion voy a asumir que para cualquier par q , p primos existe un par (e,d) tal que e*d mod ((p-1)(q-1)) = 1

claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = encontrarClaves (m-1) 1 n m 1
            where m = (p-1)*(q-1)
                  n = p*q

-- le asigno a la clave publica el valor m-1 porque es el valor mas alto que puede adquirir.
encontrarClaves :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
encontrarClaves e d n m k | mod x m == 1 = (e, d, n) -- consegui un numero x tal que mod e*d m = 1 por lo que tengo las dos claves que queria
                          | e < div (y+1) d = encontrarClaves (m-1) (d+1) n m k -- ya pase por todos los posibles valores de e por lo que vuelvo a empezar y le sumo 1 a D
                          | d > (y+1) = encontrarClaves (m-1) 1 n m (k+1) -- ya pase por todos los posibles valores de d por lo que me fijo con el siguiente multiplo de m
                          | otherwise = encontrarClaves (e-1) d n m k -- si no cumple con ninguna de las anteriores le resto 1 y empiezo denuevo
                              where x = e*d
                                    y = k*m

--(5)

codificador :: Clpub -> Mensaje -> Cifrado
codificador (e, n) msg = modExpRecursivo e n (aEnteros msg) []

-- esta funcion agarra un set [Integer] y devuelve todos los valores del set pasados por la funcion modExp
modExpRecursivo :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
modExpRecursivo e n set crip | null set = crip -- cuando no hay mas elementos termina la funcion devolviendome los valores procesados
                             | otherwise = modExpRecursivo e n a b
                                where a = tail set -- le saco el primer elemento a al set para que la recursion pueda analizar el siguiente
                                      b = crip ++ [modExp c e n] -- agrego el valor procesado a la lista
                                      c = head set -- pido que me de el primer valor de la lista para procesarlo

--(7)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d, n) crip = aChars (modExpRecursivo d n crip []) 
-- uso la funcion modExpRecursivo para decodificar cada uno de los valores del set y despues a los valores decodificados los paso a mensaje de texto con aChars
