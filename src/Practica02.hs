module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

-- Funcion auxiliar
--Longitud de una lista
--(usado en toDecimal)
long :: [a] -> Int
long [] = 0
long (_:xs) = 1 + long xs

--Pasa de Binario a Decimal
toDecimal :: Binario -> Int
toDecimal [] = error "No se puede pasar a decimal"
toDecimal [O] = 0
toDecimal [I] = 1
toDecimal (I:xs) = (2 ^ long xs) + toDecimal xs
toDecimal (O:xs) = toDecimal xs

--Pasa de Decimal a Binario
toBin :: Int -> Binario
toBin 0 = [O]
toBin 1 = [I]
toBin n = if n `mod` 2 == 0 then toBin (n `div` 2) ++ [O] else toBin (n `div` 2) ++ [I]

--Suma dos Binarios y devuelve un Binario
suma :: Binario -> Binario -> Binario
suma xs [] = xs
suma [] ys = ys
suma xs ys = toBin(toDecimal xs + toDecimal ys)

--LISTAS

-- Función del ejercico 1 de la seccion "Listas", que verifica si una lista es palíndromo
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reversa xs

--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = concatena (diferencia xs ys) (diferencia ys xs)

--Conjunto potencia
-- Funcion del ejercicio 3 de la seccion "Listas", que calcula el conjunto potencia 
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]  
conjuntoPotencia (x:xs) = concatena subconjuntosSinX subconjuntosConX
  where
   -- Subconjuntos que no incluyen x
    subconjuntosSinX = conjuntoPotencia xs
    -- Subconjuntos que sí incluyen x
    subconjuntosConX = agregaATodas x subconjuntosSinX


--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud
longitud :: ListaPar a b -> Int
longitud [] = 0
longitud ((_,_):xs) = 2 + longitud xs

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap _ _ [] = []
myMap f g ((a,b):xs) = (f a, g b):myMap f g xs

--Sumar pares
sumaPares :: (Num a, Num b) => ListaPar a b -> (a,b)
sumaPares [] = (0, 0)
sumaPares ((a,b):xs) = (a + sumaPrimer xs, b + sumaSegundo xs)

--Filter pares
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter _ [] = []
myFilter f ((a, b):xs) = if f (a,b) then ((a,b):myFilter f xs) else (myFilter f xs)

--Funciones auxiliares para LISTAS

-- Reversa de una lista
-- Se utiliza en la funcion palindromo
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concatena (reversa xs) [x] --O bien reversa xs ++ [x]

-- Concatena dos listas
-- Se utiliza en la funcion diferenciaSimetrica
-- Se utiliza en la funcion conjuntoPotencia
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = (x:concatena xs ys)

-- Busca un elemento en una lista
-- Se utiliza en la funcion auxiliar diferencia
contiene :: (Eq a) => [a] -> a -> Bool
contiene [] _ = False
contiene (x:xs) y = if x == y then True else contiene xs y

-- Diferencia de dos listas (elementos en xs pero no en ys)
-- Se utiliza en la funcion diferenciaSimetrica
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia (x:xs) ys
    | contiene ys x = diferencia xs ys
    | otherwise     = x : diferencia xs ys

-- Agrega un elemento al inicio de cada lista en una lista de listas
-- Se utiliza en la funcion conjuntoPotencia
agregaATodas :: a -> [[a]] -> [[a]]
agregaATodas _ [] = []
agregaATodas x (ys:yss) = (x:ys) : agregaATodas x yss


--Funciones auxiliares para LISTAS DE LONGITUD PAR

-- sumaPrimer sirve para obtener el valor solo de los primeros elementos de la lista par
-- Se utiliza para la funcion sumaPares
sumaPrimer :: (Num a) => ListaPar a b -> a
sumaPrimer [] = 0
sumaPrimer ((a, _):xs) = a + sumaPrimer xs

-- sumaSegundo sirve para obtener el valor solo de los segundos elementos de la lista par
-- Se utiliza para la funcion sumaPares
sumaSegundo :: (Num b) => ListaPar a b -> b
sumaSegundo [] = 0
sumaSegundo ((_, b):xs) = b + sumaSegundo xs



----------------------------------------------
--conjuntoPotencia :: [a] -> [[a]]
--conjuntoPotencia [] = [[]]
--conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ (conjuntoPotencia xs)