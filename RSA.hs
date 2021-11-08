module RSA where
import Tipos
import Aritmetica

--(4)
-- en esta funcion voy a asumir que para cualquier par q , p primos existe un par (e,d) tal que e*d mod ((p-1)(q-1)) = 1

claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d , n)
            where m = (p-1)*(q-1)
                  n = p*q
                  e = m - 1
                  d = inversoMultiplicativo (m-1) m
-- LE ASIGNO M-1 A E PORQUE ES EL VALOR MAS GRANDE QUE PUEDE ADQUIRIR Y A D EL INVERSO MULTIPLICATIVO DE E

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
