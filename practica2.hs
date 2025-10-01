module LabDis where

--FUNCIONES AUXILIARES 

--Reversa de una lista
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concatena (reversa xs) [x] --O bien reversa xs ++ [x]

concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = (x:concatena xs ys)

contiene :: (Eq a) => [a] -> a -> Bool
contiene [] _ = False
contiene (x:xs) y = if x == y then True else contiene xs y

-- Diferencia de dos listas (elementos en xs pero no en ys)
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia (x:xs) ys
    | contiene ys x = diferencia xs ys
    | otherwise     = x : diferencia xs ys

-- Agrega un elemento al inicio de cada lista en una lista de listas
agregaATodas :: a -> [[a]] -> [[a]]
agregaATodas _ [] = []
agregaATodas x (ys:yss) = (x:ys) : agregaATodas x yss


--FUNCIONES DE LA PRACTICA 2


-- Función del ejercico 1 de la seccion "Listas", que verifica si una lista es palíndromo
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reversa xs

-- Funcion del ejercicio 2 de la seccion "Listas", que calcula la diferencia simetrica de dos listas 
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = concatena (diferencia xs ys) (diferencia ys xs)

-- Funcion del ejercicio 3 de la seccion "Listas", que calcula el conjunto potencia 
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]  
conjuntoPotencia (x:xs) = concatena subconjuntosSinX subconjuntosConX
  where
   -- Subconjuntos que no incluyen x
    subconjuntosSinX = conjuntoPotencia xs
    -- Subconjuntos que sí incluyen x
    subconjuntosConX = agregaATodas x subconjuntosSinX


