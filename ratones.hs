data Animal= Raton {nombre :: String, edad :: Double, peso :: Double, enfermedades :: [String]} deriving Show

cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]


-- 1) Hacer 4 funciones de modificación del ratón: 
-- modificarNombre, 
-- modificarEdad, 
-- modificarPeso, 
-- modificarEnfermedades. 
-- Deben recibir una función y un ratón, y devolver el ratón modificado. 

modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre funcion raton = raton {nombre=(funcion.nombre)raton}
--Invocamos: modificarNombre (++"as") cerebro

modificarEdad :: (Double -> Double) -> Animal -> Animal
modificarEdad funcion raton = raton {edad=(funcion.edad)raton}
--Invocamos: modificarEdad (*5) cerebro

modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso funcion raton = raton {peso=(funcion.peso)raton}
--Invocamos: modificarPeso  (*5) cerebro

modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades funcion raton = raton{enfermedades=(funcion.enfermedades)raton}
--Invocamos: modificarEnfermedades (++["as"]) cerebro

-- 2)Existen distintos tipos de hierbas que afectan de diferentes maneras al ratón. Definir dichas hierbas:

-- a. hierbaBuena, que rejuvenece al ratón a la raíz cuadrada de su edad.
    
-- b. hierbaVerde: elimina una enfermedad dada.
    
-- c. alcachofa: hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
    
-- d. hierbaMagica: hace que el ratón pierda todas sus infecciones y quede con 0 años de edad.
type Hierba = (Animal -> Animal)

hierbaBuena :: Hierba
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Hierba
hierbaVerde enfermedad raton = modificarEnfermedades (curarEnfermedad enfermedad) raton

curarEnfermedad :: String -> [String] -> [String]
curarEnfermedad enfermedad enfermedades = filter (\enf -> enf /= enfermedad) enfermedades

alcachofa :: Hierba
alcachofa raton | (peso raton) > 2 = modificarPeso (*0.9) raton
                | otherwise = modificarPeso (*0.95) raton 

hierbaMagica :: Hierba
hierbaMagica raton = modificarEdad (\_ -> 0) (foldr hierbaVerde raton enfermedadesInfecciosas)

-- 3)Medicamentos:
-- a)Hacer la función medicamento, que recibe una lista de hierbas, un ratón,
--  y administra al ratón todas las hierbas.
medicamento :: [Hierba] -> Animal -> Animal
medicamento hierbas raton = foldl (\rat hierba -> hierba rat) raton hierbas
--La invocamos así: medicamento [hierbaMagica,alcachofa,hierbaBuena,hierbaVerde "sarampión"] cerebro

-- b)Hacer antiAge que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa.
antiAge :: Animal -> Animal
antiAge raton = medicamento [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa] raton

-- c)Hacer reduceFatFast (que viene en distintas potencias) y es un medicamento compuesto por 
-- una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.
reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia raton = medicamento ([hierbaVerde "obesidad"] ++ replicate potencia alcachofa) raton

-- d) Hacer la función hierbaMilagrosa,
--  que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas.
hierbaMilagrosa :: Animal -> Animal 
hierbaMilagrosa raton = medicamento (map hierbaVerde enfermedadesInfecciosas) raton