-- El Gran DT - 2025 - Parcial Funcional
-- Tema 1
-- Un importante y sudoroso personaje del fútbol nos ha pedido que modelemos los requerimientos 
-- para la importante liga que tiene a su cargo. Sabemos que un jugador tiene nombre, 
-- una velocidad medida en km/h, una habilidad que va de 0 a 100, 
-- el puesto en el que juega: “arquero”, “defensor”, “volante” o “delantero” y también 
-- se registran los partidos donde jugó, del primero al último: en él se marca minutos jugados y 
-- goles convertidos. Si un jugador estuvo en el banco de suplentes tendrá 0 minutos jugados.

data Jugador = Jugador {
    nombre::String, 
    velocidad::Double, 
    habilidad::Double,
    puesto::String,
    partidos::[Partido]
    } deriving Show
data Partido = Partido {
    minutosJugados::Double,
    golesConvertidos::Integer
    } deriving Show

partido1 = Partido 90.0 1
partido2 = Partido 70.0 2
partido3 = Partido 120.0 3
partido4 = Partido 65.0 1
partido5 = Partido 85.0 0
partido6 = Partido 12.0 0

messi = Jugador "Lionel Andres Messi" 32.0 95.0 "Delantero" [partido1, partido2, partido3]
dePaul = Jugador "Rodrigo De Paul" 25.0 80.0 "Mediocampo" [partido4, partido5]
fabra = Jugador "Frank Fabra" 28 12 "Defensor" [partido6]

team = [messi, dePaul, fabra]

-- Punto 1: Equipos (3 puntos)
-- En este punto no puede utilizar funciones auxiliares, solo composición y aplicación parcial. No puede utilizar recursividad.

-- a) Queremos saber los nombres de los jugadores de un equipo que jugaron todos los partidos 
-- al menos una cantidad de minutos, donde esa cantidad de minutos sea parametrizable. 
-- Por ejemplo, si elegimos que la cantidad de minutos sea 45, puede haber jugado 45, 46 ó los 90, 
-- eso alcanza.
type Equipo = [Jugador]

siempreJugoMinutos :: Double -> Equipo -> [String]
siempreJugoMinutos minutos = map nombre . filter (all ((>= minutos) . minutosJugados) . partidos)


-- b) Queremos si hay algún jugador de un equipo cuyo nombre empieza con una letra, por ejemplo 'F'.
jugadorEmpiezaCon :: Char -> Equipo -> Bool
jugadorEmpiezaCon letra = any ((==) letra . head . nombre)


-- Punto 2: Técnicos (3 puntos)
-- Presentamos ahora a los técnicos, que con sus tácticas modifican a los jugadores
-- Bielsa le agrega un 50% de velocidad a los jugadores pero también baja 10 puntos su habilidad 
    -- (todo no se puede)
-- Menotti le agrega un "Mr. " al nombre del jugador y aumenta una cantidad de puntos su habilidad,
    -- donde esa habilidad se puede parametrizar
-- Bertolotti hace lo mismo que Menotti, solo que aumenta siempre 10 su habilidad
-- Van Gaal no afecta a los jugadores, siguen jugando igual que siempre

-- Indique cómo haría en la consola para decir que un jugador fue entrenado primero por Bielsa, luego por Menotti y finalmente por Van Gaal. No importa el resultado sino cómo es la invocación en el REPL.

type Tecnico = (Jugador -> Jugador)

cambiarVelocidad :: (Double -> Double) -> Jugador -> Jugador
cambiarVelocidad operacion jugador = jugador {velocidad=operacion (velocidad jugador)}

cambiarHabilidad :: (Double -> Double) -> Jugador -> Jugador
cambiarHabilidad operacion jugador = jugador {habilidad=operacion (habilidad jugador)}

cambiarNombre :: (String -> String) -> Jugador -> Jugador
cambiarNombre operacion jugador = jugador {nombre=operacion (nombre jugador)}

bielsa :: Tecnico
bielsa jugador = (cambiarVelocidad (*1.5) . cambiarHabilidad (+ (-10)) ) jugador
-- >bielsa messi

menotti :: Double -> Tecnico
menotti puntos jugador = (cambiarNombre ("Mr "++) . cambiarHabilidad (+ puntos) ) jugador
-- >menotti 80 messi

bertolotti :: Tecnico
bertolotti jugador = menotti 10.0 jugador
-- >bertolotti messi

vangaal :: Tecnico
vangaal jugador = jugador
-- >vangaal messi

--Si fue por los 3 es
-- >vangaal $ menotti 15 $ bielsa messi


-- Punto 3: Mejora (2 puntos)
-- Sabemos que un jugador es bueno si tiene más habilidad que velocidad, 
-- o bien si es volante. Queremos saber si un técnico mejora a un equipo, 
-- esto se da si después de entrenar a todos los jugadores de un equipo tiene más jugadores buenos 
-- que antes de entrenarlos.

mejoraEquipo :: Tecnico -> Equipo -> Bool
mejoraEquipo tecnico equipo = cantBuenosJugadores (map tecnico equipo) > cantBuenosJugadores equipo

cantBuenosJugadores :: Equipo -> Int
cantBuenosJugadores equipo = length $ filter esBueno equipo

esBueno :: Jugador -> Bool
esBueno jugador | habilidad jugador > velocidad jugador = True
                | puesto jugador == "volante" = True
                | otherwise = False

-- Punto 4: Es imparable (2 puntos)
-- Queremos saber si un jugador es imparable. 
-- Esto se da si con el correr de los partidos va metiendo la misma cantidad de goles o más.

-- Por ejemplo un jugador que en el primer partido no metió goles, en el segundo metió 2, 
-- en el tercero metió 2 y en el cuarto metió 4 es imparable.
-- En el caso de un jugador que en el primer partido no metió goles, 
-- en el segundo hizo un gol y en el tercero no metió goles, no es imparable.

esImparable :: Jugador -> Bool
esImparable jugador = golesConsecutivos (partidos jugador)

golesConsecutivos :: [Partido] -> Bool
golesConsecutivos [] = False
golesConsecutivos [_] = False --si tiene 0 o 1 partido no le sumamos nada
golesConsecutivos [p1,p2] = golesConvertidos p1 <= golesConvertidos p2
golesConsecutivos (cabeza:cuello:cola) = golesConsecutivos [cabeza,cuello] && golesConsecutivos (cuello:cola)