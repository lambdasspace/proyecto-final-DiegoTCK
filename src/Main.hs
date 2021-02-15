module Main where

import System.IO
import Data.List
import Data.Char
import Trie

-- Importando el diccionario.
diccionario :: FilePath
diccionario = "diccionario.txt"

-- fichero del texto de prueba.
sv :: FilePath
sv = "Texto_prueba.txt"

-- Funcion que nos dice si una unica palabra esta en el diccionario.
buscapalabra :: [Char] -> [Arbol] -> Bool
buscapalabra [] arbol = True
buscapalabra [x] arbol = case arbol of
	[Hoja] -> False
	[Flor y lista] -> case lista of
		[Hoja] -> if x == y then True else False
		[Flor z subl] -> False
		(a:as) -> if x == y && hayHoja (a:as) then True else buscapalabra [x] [Flor y as]
	(a:as) -> if esFlor x a then buscapalabra [x] [a] else False || buscapalabra [x] as
buscapalabra (a:as) arbol = case arbol of
	[Hoja] -> False
	[Flor x lista] -> case lista of
		[Hoja] -> if a == x then buscapalabra as [Hoja] else False
		[Flor y subl] -> if a == x then True && buscapalabra as lista else False
		(b:bs) -> if a == x then buscapalabra as [b] || buscapalabra as lista else False
	(x:xs) -> if esFlor a x then buscapalabra as [x] else False || buscapalabra (a:as) xs

-- Funcion que nos dice si hay hojas en una lista
hayHoja :: [Arbol] -> Bool
hayHoja [] = False
hayHoja arbol = case arbol of
	[Hoja] -> True
	[Flor x (a:as)] -> hayHoja [a] && hayHoja as
	(a:as) -> hayHoja [a]

-- Funcion que carga el archivo de texto que vamos a usar.
cargaTxt :: IO [String]
cargaTxt = do { contenido <- readFile sv;
			  	return $ words contenido;
			  }

-- Funcion que construye el arbol del diccionario.
dict :: IO Arbol
dict = do { putStr ("Cargando el diccionario." ++ diccionario ++ "\n");
			contenedor <- (readFile diccionario);
			let palabras = (lines contenedor) in do {
				putStr (show (length palabras));
				putStr " palabras leidas\n";
				let abc = semilla palabras in do {
					return abc;
				}
			}
		  }

-- Funcion que agrega una nueva palabra al diccionario.
nuevaP :: IO ()
nuevaP = do { putStr "\nInserta la nueva palabra: ";
			  cadena <- getLine;
			  appendFile diccionario ("\n" ++ cadena);
			  putStr "\nPalabra guardada en el Diccionario local.";
			}
