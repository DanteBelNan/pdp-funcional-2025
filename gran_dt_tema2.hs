-- El Gran DT - 2025 - Parcial Funcional
-- Tema 2
-- Un importante y sudoroso personaje del fútbol nos ha pedido que modelemos los requerimientos para la importante liga que tiene a su cargo. Sabemos que un jugador tiene nombre, una velocidad medida en km/h, una habilidad que va de 0 a 100, el puesto en el que juega: “arquero”, “defensor”, “volante” o “delantero” y también se registran los partidos donde jugó, del primero al último: en él se marca minutos jugados y goles convertidos. Si un jugador estuvo en el banco de suplentes tendrá 0 minutos jugados.

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
partido4 = Partido 65.0 2
partido5 = Partido 85.0 1
partido6 = Partido 12.0 0

messi = Jugador "Lionel Andres Messi" 32.0 95.0 "volante" [partido1, partido2, partido3]
dePaul = Jugador "Rodrigo De Paul" 25.0 80.0 "volante" [partido4, partido5]
fabra = Jugador "Frank Fabra" 28 12 "Defensor" [partido6]

team = [messi, dePaul, fabra]

-- Punto 1: Equipos (3 puntos)
-- En este punto no puede utilizar funciones auxiliares, solo composición y aplicación parcial. No puede utilizar recursividad.

-- a) Queremos saber cuántos jugadores de un equipo marcaron goles en todos los partidos. 
type Equipo = [Jugador]
cantJugadoresMarcanSiempre :: Equipo -> Int
cantJugadoresMarcanSiempre equipo = length $ filter (all ((>0).golesConvertidos) . partidos) equipo 

-- b)
-- Queremos saber si todos los jugadores de un equipo que tienen más de un mínimo de puntos de habilidad
-- son volantes, donde ese mínimo es configurable. Ojo: si elijo que el mínimo sea 35, 
-- si hay 2 volantes que tienen 40 y 50 puntos de habilidad y un delantero con 50 puntos de habilidad,
-- la condición no se cumple: todos los jugadores deben ser únicamente volantes.

volantesHabilidosos :: Double -> Equipo -> Bool
volantesHabilidosos skillRequired equipo = all ((== "volante") . puesto) $ (filter ((>skillRequired) . habilidad)) equipo 

-- Punto 2: Técnicos (3 puntos)
-- Presentamos ahora a los técnicos, que con sus tácticas modifican a los jugadores
-- Gago hace que los volantes jueguen de defensores y los delanteros de volantes 
    -- (el resto queda en su posición)
-- Bilardo hace que jueguen un partido amistoso que termina 0 a 0
    -- (hay que agregarlo al jugador que juega todo el partido) y 
    -- también aumenta 5 puntos su habilidad si Bilardo está nervioso, ó 10 si Bilardo está tranquilo
-- Fatigatti hace lo mismo que Bilardo pero siempre está tranquilo
-- Klopp no afecta a los jugadores, siguen jugando igual que siempre

-- Indique cómo haría en la consola para decir que un jugador fue entrenado primero por Gago, luego por Bilardo y finalmente por Klopp. No importa el resultado sino cómo es la invocación en el REPL.

cambiarPuesto :: String -> Jugador -> Jugador
cambiarPuesto nuevoPuesto jugador = jugador {puesto=nuevoPuesto}

modificarPartidos :: ([Partido] -> [Partido]) -> Jugador -> Jugador
modificarPartidos operacion jugador = jugador {partidos=operacion (partidos jugador)}

modificarHabilidad :: (Double -> Double) -> Jugador -> Jugador
modificarHabilidad operacion jugador = jugador {habilidad=operacion (habilidad jugador)}

type Tecnico = Jugador -> Jugador

gago :: Tecnico
gago jugador | puesto jugador == "volante" = cambiarPuesto "defensor" jugador
             | puesto jugador == "delantero" = cambiarPuesto "volante" jugador
             | otherwise = jugador

bilardo :: Bool -> Tecnico
bilardo estaTranquilo jugador = bilardoTranqui estaTranquilo $ modificarPartidos (++[(Partido 90.0 0)]) jugador

bilardoTranqui :: Bool -> Jugador -> Jugador
bilardoTranqui estaTranquilo jugador | estaTranquilo = modificarHabilidad (+10) jugador
                                     | otherwise = modificarHabilidad (+5) jugador

fatigatti :: Tecnico
fatigatti jugador = bilardo True jugador

klopp :: Tecnico
klopp jugador = jugador

-- La invocacion pedida:
-- klopp (bilardo True $ gago messi)

--Punto 3: Buena enseñanza (2 puntos)
-- Sabemos que un jugador es bueno si tiene más habilidad que velocidad, o bien si es volante. 
-- Queremos saber si un jugador tiene una buena enseñanza, esto se da si después de que lo entrenan 
-- una serie de técnicos termina siendo un jugador bueno (no importa si antes ya lo era).
-- En este punto no puede utilizar recursividad.

buenLearning :: [Tecnico] -> Jugador -> Bool
buenLearning tecnicos jugador = esBueno $ foldl (\sem elem -> elem sem) jugador tecnicos

esBueno :: Jugador -> Bool
esBueno jugador | habilidad jugador > velocidad jugador = True
                | puesto jugador == "volante" = True
                | otherwise = False

-- Punto 4: Regularidad (2 puntos)
-- Dado un jugador, queremos saber si perdió regularidad. 
-- Esto se da si con el correr de los partidos juega menos minutos.
-- Por ejemplo, un jugador que en el primer partido jugó 10 minutos, y en el segundo 20 
-- no perdió regularidad. Si juega 10 minutos en el segundo partido tampoco perdió regularidad.
-- En cambio uno que jugó 90 minutos los primeros 3 partidos y luego jugó 80 minutos, 
-- perdió regularidad.
-- En este punto debe utilizar recursividad.

perdioRegularidad :: Jugador -> Bool
perdioRegularidad jugador = minutosDecrecientes (partidos jugador)

minutosDecrecientes :: [Partido] -> Bool
minutosDecrecientes [] = False
minutosDecrecientes [_] = False
minutosDecrecientes [p1,p2] = minutosJugados p1 > minutosJugados p2
minutosDecrecientes (cabeza:cuello:cola) = minutosDecrecientes [cabeza,cuello] || minutosDecrecientes (cuello:cola)