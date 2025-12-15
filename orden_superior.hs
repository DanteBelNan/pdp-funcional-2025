pares numeros = [num | num <- numeros, even num]
divisiblesPor n numeros = [num | num <- numeros, esDivisiblePor n num]
esDivisiblePor n num = (mod num n) == 0
mayoresA n numeros = [num | num <- numeros, num > n]

--Si quisieramos unificar esto en una funcion sola que reciba una lista y le aplique una funcion
-- usamos orden superior
type Funcion = (Integer -> Bool)
seleccionar :: Funcion -> [Integer] -> [Integer]
seleccionar criterio numeros = [num | num <- numeros, criterio num]
--Ahora con seleccionar even [1..10] nos trae todos los pares del 1 al 10

--Con seleccionar (esDivisiblePor 3) [1..10] nos funciona tambien


--Algunas funciones genericas son
filter' :: (a -> Bool) -> [a] -> [a]
filter' criterio lista = [ elemento | elemento <- lista, criterio elemento]
--filter' (even.fst) [(2,5), (5,8), (4,6)]

map' :: (a -> b) -> [a] -> [b]
map' criterio lista = [criterio elemento | elemento <- lista]

--Este tercer caso aplica un filtro y luego le hace un cambio a los filtrados
filter'' :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filter'' criterio criterio2 lista = [criterio2 elemento | elemento <- lista, criterio elemento]
-- filter'' even (\x -> x + 1) [2,3,4,5,6]