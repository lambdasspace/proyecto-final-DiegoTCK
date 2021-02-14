module Main where

import System.IO
import Data.Char
import Trie

-- Importando el diccionario.
diccionario :: FilePath
diccionario = "diccionario.txt"

-- Funcion que nos dice si una unica palabra esta en el diccionario.
buscaPalabra :: [Char] -> [Arbol] -> Bool
buscaPalabra [] arbol = True
buscaPalabra [x] arbol = case arbol of
	[Hoja] -> False-- Funcion que nos dice si una unica palabra esta en el diccionario.
buscaPalabra :: [Char] -> [Arbol] -> Bool
buscaPalabra [] arbol = True
buscaPalabra [x] arbol = case arbol of
	[Hoja] -> False
	[Flor y lista] -> case lista of
		[Hoja] -> if x == y then True else False
		[Flor z subl] -> False
		(a:as) -> if x == y && hayHoja (a:as) then True else buscaPalabra [x] [Flor y as]
	(a:as) -> if esFlor x a then buscaPalabra [x] [a] else False || buscaPalabra [x] as
buscaPalabra (a:as) arbol = case arbol of
	[Hoja] -> False
	[Flor x lista] -> case lista of
		[Hoja] -> if a == x then buscaPalabra as [Hoja] else False
		[Flor y subl] -> if a == x then True && buscaPalabra as lista else False
		(b:bs) -> if a == x then buscaPalabra as [b] || buscaPalabra as lista else False
	(x:xs) -> if esFlor a x then buscaPalabra as [x] else False || buscaPalabra (a:as) xs
	[Flor y lista] -> case lista of
		[Hoja] -> if x == y then True else False
		[Flor z subl] -> False
		(a:as) -> if x == y && hayHoja (a:as) then True else buscaPalabra [x] [Flor y as]
	(a:as) -> if esFlor x a then buscaPalabra [x] [a] else False || buscaPalabra [x] as
buscaPalabra (a:as) arbol = case arbol of
	[Hoja] -> False
	[Flor x lista] -> case lista of
		[Hoja] -> if a == x then buscaPalabra as [Hoja] else False
		[Flor y subl] -> if a == x then True && buscaPalabra as lista else False
		(b:bs) -> if a == x then buscaPalabra as [b] || buscaPalabra as lista else False
	(x:xs) -> if esFlor a x then buscaPalabra as [x] else False || buscaPalabra (a:as) xs