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
-- Definiciones de datos para ejercicios 2, 4 y 5:
type Nombre = String
type Notas = [Int]
data Persona = Alumno { nombre :: Nombre, notas :: Notas } deriving Show

-- 4) Función aprobó
-- Definir la función aprobó/1, que dado un alumno devuelve True si el alumno aprobó.
-- Aclaración: Se dice que un alumno aprobó si todas sus notas son 6 o más.

-- 5) Función aprobaron
-- Definir la función aprobaron/1, que dada una lista de alumnos, devuelve los nombres de los alumnos que aprobaron.

-- 6) Función productos
-- Definir la función productos que dado una lista de nombres de productos y una lista de precios, devuelve una lista de tuplas.
-- Consigna:
-- Definirla usando zip.
-- Definirla usando zipWith.