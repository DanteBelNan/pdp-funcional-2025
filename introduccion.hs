siguiente :: Integer -> Integer
siguiente nro = nro + 1

--Si es par da su consecutivo si es impar su doble
calcular :: Integer -> Integer
calcular nro | even nro = nro + 1
             | otherwise = nro * 2

doble :: Integer -> Integer
doble n = n * 2

calcular2 :: Integer -> Integer
calcular2 n | even n = siguiente n
            | otherwise = doble n

sumar :: (Integer, Integer) -> Integer --Usamos una tupla para pasarle datos
sumar (n1,n2) = n1 + n2

sumar2 :: (Integer,Integer) -> Integer
sumar2 (n1,n2) = (+) n1 n2 --Le aplicamos la notación al + para pasarle los dos argumentos


--Podemos evitar mencionar el tipo de dato para hacerlo generico
mostrarSegundo :: (a,b,c) -> b
mostrarSegundo (_,elem,_) = elem

--Lambdas
aplicarLambda :: Integer -> Integer
aplicarLambda n = (\x -> x + 2) n



--Ejercicio
-- 1)
-- Definir la función calcular’, que recibe una tupla de 2 elementos
-- Esta devuelve una nueva tupla según las siguientes reglas:
--  si el primer elemento es par lo duplica; si no lo deja como está
--  si el segundo elemento es impar le suma 1; si no deja como está

calcular' :: (Integer,Integer) -> (Integer,Integer)
calcular' (n1,n2) = (duplicaPar n1, sumaImpar n2)

duplicaPar :: Integer -> Integer
duplicaPar n | even n = n * 2
             | otherwise = n
sumaImpar :: Integer -> Integer
sumaImpar n | odd n = n + 1
            | otherwise = n

-- 2)
-- Definir las funciones booleanos de OR y AND sin usar funcioned predefinidas
and' :: Bool -> Bool -> Bool
and' first second | not first = False
				  | not second = False
   				  | otherwise = True

or' :: Bool -> Bool -> Bool
or' first second | first = True
                 | second = True
                 | otherwise = False

--En este otro caso le damos por default que hacer en algunos casos
or'' :: Bool -> Bool -> Bool
or'' True second = second
or'' first True = first
or'' _ _ = False

-- 3)
--Definir la función notaMaxima que dado un alumno devuelva
--la máxima nota del alumno. (Nota resolverlo sin guardas).
type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_,n1,n2,n3) = max n1 (max n2 n3)

-- 4) 
--Definir la función cuadruple reutilizando la función doble.
cuadruple :: Integer -> Integer
cuadruple n = doble (doble n)

-- 5) 
--Definir la función esMayorA, que verifique si
--el doble del siguiente de la suma entre 2 y un número es mayor a 10. 
esMayorA :: Integer -> Bool
esMayorA n = 2 * (siguiente (n + 2)) > 10

--6) 
--Dar expresiones lambda que sean equivalentes a las siguientes expresiones:
--triple
triple :: Integer -> Integer
triple n = (\x -> x * 3) n
--siguiente
siguiente' :: Integer -> Integer
siguiente' n = (\x -> x + 1) n
--suma
suma :: Integer -> Integer -> Integer
suma n1 n2 = (\x y -> x + y) n1 n2
--sumarDos
sumarDos :: Integer -> Integer
sumarDos n = (\x -> x +2) n


--Pattern matching, tal como hicimos en or'' sirve para dar por sentado algunos atributos
signo :: Integer -> Integer
signo 0 = 0
signo n | n > 0 = 1
        | otherwise = -1
        -- para llamar a un negativo hacemos "signo (-1)"

--Podemos concatenar operaciones de manera tal que se ejecuten una tras otra
-- (even . (+5)) 3
-- Va a hacer 3 + 5 y ese resultado ver si es par

-- (odd . siguiente . suma 7) 5 
-- Va a hacer 5 aplicado a suma con 7, a ese le va a aplicar el siguiente y ahí ver si es impar.

-- Ejercicio: armar una función que dado un punto en el plano, nos de su distancia al origen
type Punto = (Double,Double)
type Distancia = Double

distanciaOrigen :: Punto -> Distancia
distanciaOrigen (x,y) = sqrt(x*x + y*y)

--Si quisieramos hace rque esta distancia funcione tambien para espacios, es decir 3D
--Usamos datas
data Punto' = Plano{x::Double,y::Double} |
              Espacio{x::Double,y::Double,z::Double} deriving Show

distanciaOrigen' :: Punto' -> Distancia
distanciaOrigen' (Plano x y) = sqrt(x*x + y*y)
distanciaOrigen' (Espacio x y z) = sqrt(x*x + y*y + z*z)
