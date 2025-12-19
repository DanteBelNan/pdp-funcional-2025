-- 1) Función find’
-- Resolver la función find’ que encuentra el primer elemento que cumple una condición.
-- Restricción: No se puede resolver con recursividad. Si ningún elemento cumple la condición, dejar que falle.
find' :: (a -> Bool) -> [a] -> a

find' criterio lista = (head.filter criterio) lista

-- 1.1) Aplicación al dominio Político
-- Aprovechar la función find’ para aplicarla a este dominio.
-- Definiciones de datos:
data Politico = Politico { proyectosPresentados :: [String], sueldo :: Integer, edad :: Int } deriving Show
politicos = [ 
    Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, 
    Politico ["tratar de reconquistar luchas sociales"] 10000 63, 
    Politico ["tolerancia 100 para delitos"] 15500 49 
 ]
-- Consigna: Queremos encontrar (usando find' y sin generar funciones nuevas, resolviendo desde la consola):
-- a) Un político joven (menos de 50 años).
--find' ((<50).edad) politicos
-- b) Alguno que haya presentado más de 3 proyectos.
--find' ((>3).length.proyectosPresentados) politicos
-- c) Alguno que haya presentado algún proyecto que tenga más de 3 palabras.

-- 2) Función promediosAlumnos
-- Definir la función promediosAlumnos/1, que dada una lista de alumnos devuelve una lista de tuplas que tenga el alumno y el promedio (Consideramos la división entera para el promedio y usamos la función div).
-- Definiciones de datos para ejercicios 2,3 , 4 y 5 dada por el docente:
type Nombre = String
type Notas = [Int]
data Persona = Alumno { nombre :: Nombre, notas :: Notas } deriving Show

promedio :: Notas -> Int
promedio [] = 0
promedio notas = div (sum notas) (length notas)

promedioAlumnos :: [Persona] -> [(String,Int)]
promedioAlumnos alumnos = map (\(Alumno nombre notas) -> (nombre,promedio notas)) alumnos



-- 3) Función promediosSinAplazos/1, que dada una lista de listas, devuelvan la lista 
-- de los promedios de cada lista-elemento, excluyendo los que sean menores a 6 que no se cuentan

promedioSinAplazos :: [Persona] -> [(String,Int)]
promedioSinAplazos alumnos = map (\(Alumno nombre notas) -> (nombre, promedio(filter(>=6) notas))) alumnos

-- 4) Función aprobó
-- Definir la función aprobó/1, que dado un alumno devuelve True si el alumno aprobó.
-- Aclaración: Se dice que un alumno aprobó si todas sus notas son 6 o más.

aprobo :: Persona -> Bool
-- aprobo persona = (\(Alumno nombre notas) -> all notaAprobada notas) persona
-- notaAprobada :: Int -> Bool
-- notaAprobada nota = (>=6) nota
aprobo persona = all (>=6) (notas persona) -- un poco mejor

-- 5) Función aprobaron
-- Definir la función aprobaron/1, que dada una lista de alumnos, devuelve los nombres de los alumnos que aprobaron.
aprobaron :: [Persona] -> [String]
aprobaron alumnos = map nombre $ filter aprobo alumnos

-- 6) Función productos
-- Definir la función productos que dado una lista de nombres de productos y una lista de precios, devuelve una lista de tuplas.
-- Consigna:
-- Definirla usando zip.
tuplasZip :: [String] -> [Int] -> [(String,Int)]
tuplasZip productos precios = zip productos precios
-- Definirla usando zipWith.
tuplasZipWith :: [String] -> [Int] -> [(String,Int)]
tuplasZipWith productos precios = zipWith (\producto precio -> (producto,precio)) productos precios