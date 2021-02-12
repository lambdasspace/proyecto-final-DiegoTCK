module Trie where

-- Inicio del proyecto.

{- Definicion del arbol para el Trie
-- En si el arbol es la estructura que usare para la verificacion de las palabras. 
-- La unica manera en la que usaremos la raiz, es para construir el arbol completo
-- es decir, de todo un texto, vamos a tomar todas las palabras y vamos construyendo
-- el arbol.
-}
data Arbol = Raiz [Arbol] | Flor Char [Arbol] | Hoja deriving(Eq, Show)

-- Funcion que construye el arbol.
semilla :: [String] -> Arbol
semilla = error "Todavia no lo implemento."

{- 							Seccion de funciones auxiliares 						-}

-- Funcion auxiliar palabraNarbol transforma una lista de Char a un Arbol
palabraNarbol :: [Char] -> Arbol
palabraNarbol [] = Hoja
palabraNarbol (x:xs) = Flor x [palabraNarbol xs]

-- Funcion auxiliar que va agragando letras dentro de las ramas del arbol.
brote :: [Char] -> [Arbol] -> [Arbol]
brote [] arbol = arbol
brote [x] arbol = case arbol of
	[Hoja] -> [Hoja, Flor x [Hoja]]
	[Flor y [rama]] -> if x == y then arbol else arbol ++ brote [x] [Hoja]
brote (x:xs) arbol = case arbol of
	[Hoja] -> [Hoja, (palabraNarbol (x:xs))]
	[Flor y [rama]] -> if x == y then brote xs [rama] else arbol ++ brote (x:xs) [Hoja]

-- Funcion auxiliar que inserta una palabra en una rama del arbol.
nuevoBrote :: [Char] -> [Arbol] -> [Arbol]
nuevoBrote [] arbol = arbol
nuevoBrote [x] arbol = case arbol of
	[Hoja] -> brote [x] [Hoja]
	[Flor y [rama]] -> if x == y then [Flor y (nuevoBrote [] [rama])] else brote [x] arbol
nuevoBrote (x:xs) arbol = case arbol of
	[Hoja] -> brote (x:xs) arbol
	[Flor y [rama]] -> if x == y then [Flor y (nuevoBrote xs [rama])] else arbol ++ [palabraNarbol (x:xs)]

{- 							Seccion de arboles de prueba 						-}