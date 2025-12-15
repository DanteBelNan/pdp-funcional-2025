-- 1. Representamos las notas que sacó un alumno en dos parciales mediante un par (nota1, nota2) 
    
--     Con esto hay que hacer lo siguiente
    
--     1. Definir función esNotaBochazo, que recibe un número y devuelve True si no llega a 6, False en caso contrario, sin usar guardas
--     2. Definir la función aprobo, recibe un par e indica si una persona se sacó esas notas aprueba. se debe usar esNotaBochazo
--     3. Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 16 y ademas haberse sacado al menos 8 en cada parcial.
--     4. Escribir una consulta dado un par indica si aprobó el primer parcial usando esNotaBochaz y composición. La consulta tiene que tener esta forma Main > (… algo …) (5,8)

type Nota = (Double, Double)

esNotaBochazo :: Double -> Bool
esNotaBochazo nota = nota < 6


aprobo :: Nota -> Bool
aprobo (nota1, nota2) = (not . esNotaBochazo) nota1 && (not . esNotaBochazo) nota2

paraPromocion :: Double -> Bool
paraPromocion nota = nota >= 8

promociono :: Nota -> Bool
promociono (nota1, nota2) = paraPromocion nota1 && paraPromocion nota2

--4 (\dupla -> (not.esNotaBochazo.fst)dupla && (not.esNotaBochazo.snd)dupla)(6,4)