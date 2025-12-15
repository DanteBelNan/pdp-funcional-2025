saludar :: String -> String
saludar persona = "Hola " ++ persona ++ "!"

numerosPares numeros = [num | num <- numeros, even num]
--lo llamamos con numerosPares [1,2,3,4,5,6,etc]

-- [x^2 | x <- [1..10], even x] en lugar de pasarle la lista a mano, se genera por comprension

--[ (i,j) | i <- [1..2], j <- ['a'..'b']] tambien se genera por comprension

composiciones elementos = [ (i,j) | i <- elementos, j <- elementos ]
