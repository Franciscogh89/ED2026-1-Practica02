module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

--Longitud de una lista
--(usado en toDecimal)
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

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

--Suma dos Binarios y devuelve un Bianrio
suma :: Binario -> Binario -> Binario
suma xs [] = xs
suma [] ys = ys
suma xs ys = toBin(toDecimal xs + toDecimal ys)

--LISTAS

palindromo :: [a] -> Bool
palindromo = undefined

--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: [a] -> [a] -> [a]
diferenciaSimetrica = undefined

--Conjunto potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined


--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud
longitud :: ListaPar a b -> Int
longitud = undefined

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap = undefined

--Sumar pares
sumaPares :: ListaPar a b -> (a,b)
sumaPares = undefined

--Filter pares
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter = undefined