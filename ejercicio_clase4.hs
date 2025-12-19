-- 1- Dado una lista de flores donde cada una está representada de la siguiente forma

data Flor = Flor{
    nombreFlor::String,
    aplicacion::String,
    cantidadDeDemanda::Int
} deriving Show;

rosa = Flor "rosa" "decorativo" 120
jazmin = Flor "jazmin" "infusión" 100
violeta = Flor "violeta" "infusión" 110
orquidea = Flor "orquidea" "decorativo" 90

flores = [orquidea, rosa, violeta, jazmin]

-- a) Definir maximaFlorSegun que permite conocer el nombre de la flor que es máxima según estos criterios
-- - Cantidad de demanda
-- - Cantidad de letras de la flor
-- - El resto de la división de la cantidad demandada por 4

-- Resolverla evitando tener codigo duplicado y recursividad

maximaFlorSegun :: (Flor->Int) -> [Flor] -> String
maximaFlorSegun criterio flores = (nombreFlor . florMaxima criterio) flores

florMaxima :: (Flor -> Int) -> [Flor] -> Flor
florMaxima _ [flor] = flor
florMaxima criterio (x:xs) | (criterio x) > (criterio  . florMaxima criterio) xs = x
                           | otherwise = florMaxima criterio xs

cantidadLetrasFlor :: Flor -> Int
cantidadLetrasFlor flor = length $ nombreFlor flor

restoDivision4Demandada :: Flor -> Int
restoDivision4Demandada flor = mod (cantidadDeDemanda flor) 4

-- el primero se llama con maximaFlorSegun cantidadDeDemanda flores
-- el segundo con maximaFlorSegun cantidadLetrasFlor flores
-- el tercero con maximaFlorSegun restoDivision4Demanda flores

-- b) Dada una lista de flores determinar si están ordenadas de mayor a menor por cantidad de demanda
floresOrdenadasDemanda :: [Flor] -> Bool
floresOrdenadasDemanda [] = True
floresOrdenadasDemanda [flor] = True
floresOrdenadasDemanda [flor1,flor2] = (>) (cantidadDeDemanda flor1) (cantidadDeDemanda flor2)
floresOrdenadasDemanda (cabeza:cuello:cola) = (>) (cantidadDeDemanda cabeza) (cantidadDeDemanda cuello) && floresOrdenadasDemanda (cuello:cola)


-- 2 Dada una lista de tuplas sacar la cantidad de elementos usando fold y foldr
cantElemL :: [(a,b)] -> Integer
cantElemL lista = foldl (\sem _ -> sem + 1) 0 lista

cantElemR :: [(a,b)] -> Integer
cantElemR lista = foldr (\_ sem -> sem + 1) 0 lista

-- 3 Dada una lista de pares (empleado, gasto) , conocer el empleado mas gastador usando foldl y foldr
type Empleado = (String, Integer)
masGastadorL :: [Empleado] -> String
masGastadorL (cabeza:cola) = fst $ foldl gastoMas cabeza cola

gastoMas :: Empleado -> Empleado -> Empleado
gastoMas emp1 emp2 | (>) (snd emp1) (snd emp2) = emp1
                   | otherwise = emp2

masGastadorR :: [Empleado] -> String
masGastadorR (cabeza:cola) = fst $ foldr gastoMas cabeza cola

masGastadorL1 :: [Empleado] -> String
masGastadorL1 (empleados) = fst $ foldl1 gastoMas empleados

masGastadorR1 :: [Empleado] -> String
masGastadorR1 (empleados) = fst $ foldr1 gastoMas empleados

-- 4 Dada una lista de (empleado, gasto) conocer el gasto total usando fold y foldr
gastoTotalL :: [Empleado] -> Integer
gastoTotalL empleados = foldl (\sem (_,monto) -> sem + monto) 0 empleados

gastoTotalR :: [Empleado] -> Integer
gastoTotalR empleados = foldr (\(_,monto) sem -> sem + monto) 0 empleados

-- 5 Completar lo que necesita la siguiente función para dar 15
-- a) foldl ... 2 [(3+), (*2), (5+)]
-- Lo que le falta es  (\sem elem -> elem sem)
-- Y completar para que esta otra función de 17
-- b) foldr ... 2 [(3+), (*2), (5+)]
-- Lo que le falta es (\elem sem -> elem sem)

-- 6 Dada una lista de proyectos
type Nombre = String
type InversionInicial = Integer
type Profesionales = [String]

data Proyecto = Proy {nombre :: Nombre,inversionInicial :: InversionInicial, profesionales :: Profesionales} deriving Show

proyectos = [
    Proy "red social de arte" 200000 ["Ing en sistemas", "contador"],
    Proy "restaurante" 50000 ["cocinero", "adm de emp", "contador"],
    Proy "ventaChurros" 10000 ["cocinero"]
    ]
-- Usando foldl y foldr, determinar una función que permita conocer el máximo proyecto según.

-- 1. La inversión inicial
-- 2. El nro de profesionales
-- 3. La cantidad de palabras del proyecto

-- Por cada caso dar ejemplos de invocación y respuesta.
maximoProyectoL :: (Ord a) => (Proyecto -> a) -> [Proyecto] -> String
maximoProyectoL criterio proyectos = nombre $ foldl1 (compararProy criterio) proyectos

compararProy :: (Ord a) => (Proyecto -> a) -> Proyecto -> Proyecto -> Proyecto
compararProy funcion p1 p2 | (>) (funcion p1) (funcion p2) = p1
                           | otherwise = p2

--maximoProyectoL inversionInicial proyectos
--maximoProyectoL (length.profesionales) proyectos
--maximoProyectoL (length.words.nombre) proyectos

--con foldr seria identico, no tiene sentido escribirlo

