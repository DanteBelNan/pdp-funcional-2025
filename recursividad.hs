factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

contar :: [a] -> Integer
contar [] = 0
contar (x:xs) = 1 + contar xs

reversa :: [a] -> [a]
reversa [elem] = [elem]
reversa (x:xs) = reversa xs ++ [x]

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ semilla [] = semilla
foldl' funcion semilla (x:xs) = foldl' funcion (funcion semilla x) xs

contarListaFoldl :: [a] -> Integer
contarListaFoldl numeros = foldl' (\semilla numero -> semilla + 1) 0 numeros


foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' funcion semilla (x:xs) = funcion x (foldr' funcion semilla xs)

