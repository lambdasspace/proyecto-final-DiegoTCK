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

-- Funcion auxiliar que nos comprueba las flores.
esFlor :: Char -> Arbol -> Bool
esFlor x arbol = case arbol of
	Hoja -> False
	Flor y palo -> if x == y then True else False
	Raiz tronco -> False

-- Funcion auxiliar que va agragando letras dentro de los palos del arbol.
brote :: [Char] -> [Arbol] -> [Arbol]
brote [] arbol = arbol
brote [x] arbol = case arbol of
	[Hoja] -> [Hoja, Flor x [Hoja]]
	[Flor y palo] -> case palo of
		[Hoja] -> if x == y then arbol else [Flor y (brote [x] palo)]
		[Flor y palito] ->	if x == y 
							then [Flor y ([Hoja] ++ palo)]
							else arbol ++ [palabraNarbol [x]]
		(z:zs) ->	if x == y
					then [Flor y (Hoja:z:zs)]
					else arbol ++ [palabraNarbol [x]]
	(y:ys) -> 	if esFlor x y
				then (z:ys)
				else [y] ++ brote [x] ys where z = head (brote [x] [y]) 
brote (x:xs) arbol = case arbol of
	[Hoja] -> [Hoja, (palabraNarbol (x:xs))]
	[Flor y palo] -> case palo of
		[Hoja] ->	if x == y 
					then palo ++ [(palabraNarbol xs)]
					else arbol ++ [(palabraNarbol (x:xs))]
		[Flor z palito] ->	if x == y
							then brote xs palo
							else arbol ++ [(palabraNarbol (x:xs))]
		(z:zs) -> if x == y then brote xs (z:zs) else arbol ++ [(palabraNarbol (x:xs))]
	(y:ys) -> 	if esFlor x y
				then brote xs [y] ++ ys
				else brote (x:xs) ys

-- Funcion auxiliar que inserta una palabra en una palo del arbol.
nuevoBrote :: [Char] -> [Arbol] -> [Arbol]
nuevoBrote [] arbol = arbol
nuevoBrote [x] arbol = case arbol of
	[Hoja] -> brote [x] [Hoja]
	[Flor y palo] -> case palo of
		[Hoja] -> if x == y then arbol else arbol ++ [Flor x [Hoja]]
		[Flor z palito] ->	if x == y
							then [Flor y ([Hoja] ++ palo)]
							else arbol ++ [Flor x [Hoja]]
		(z:zs) -> 	if x == y
					then [Flor y ([Hoja] ++ palo)]
					else arbol ++ [Flor x [Hoja]]
	(y:ys) ->	if esFlor x y
				then nuevoBrote [x] [y] ++ ys
				else [y] ++ nuevoBrote [x] ys
nuevoBrote (x:xs) arbol = case arbol of
	[Hoja] -> brote (x:xs) arbol
	[Flor y palo] -> case palo of
		[Hoja] -> 	if x == y
					then [Flor y (brote xs [Hoja])]
					else arbol ++ [(palabraNarbol (x:xs))]
		[Flor z palito] ->	if x == y
							then [Flor y (nuevoBrote xs palo)]
							else arbol ++ [(palabraNarbol (x:xs))]
		(z:zs) ->	if x == y
					then [Flor y (nuevoBrote xs (z:zs))]
					else arbol ++ [(palabraNarbol (x:xs))]
	(y:ys) -> 	if esFlor x y
				then nuevoBrote (x:xs) [y] ++ ys
				else [y] ++ nuevoBrote (x:xs) ys


-- Funcion auxiliar que inserta una palabra al arbol.
rama :: String -> Arbol -> Arbol
rama [] Hoja = error "Necesito una Raiz."
rama [] (Flor x palo) = error "Necesito una Raiz."
rama (x:xs) (Raiz [tronco]) = case tronco of
	Hoja -> Raiz [(palabraNarbol (x:xs))]
	Flor y [palo] -> case palo of
		Hoja -> Raiz (nuevoBrote (x:xs) [tronco])
		Flor z [palito] -> Raiz (nuevoBrote (x:xs) [tronco])

{- 							Seccion de arboles de prueba 						-}