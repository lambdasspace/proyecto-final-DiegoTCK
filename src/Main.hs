module Main where

import System.IO
import Data.List
import Data.Char
import Trie

type Resultado = [String]

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
	[Hoja] -> True
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
	(x:xs) -> if esFlor a x then buscapalabra (a:as) [x] else buscapalabra (a:as) xs

-- Funcion que nos dice si hay hojas en una lista
hayHoja :: [Arbol] -> Bool
hayHoja [] = False
hayHoja arbol = case arbol of
	[Hoja] -> True
	[Flor x (a:as)] -> hayHoja [a] && hayHoja as
	(a:as) -> hayHoja [a]

-- Funcion que nos muestra el resultado de buscar todas las palabras del texto.
busqueda :: [String] -> Arbol -> Resultado
busqueda [] arbol = []
busqueda [x] (Raiz tronco) = if buscapalabra x tronco then [] else [x]
busqueda (x:xs) (Raiz tronco) =	if buscapalabra x tronco
								then busqueda xs (Raiz tronco)
								else [x] ++ (busqueda xs (Raiz tronco))

-- Funcion que carga el archivo de texto que vamos a usar.
cargaTxt :: IO [String]
cargaTxt = do { contenido <- readFile sv;
			  	return $ words contenido;
			  }

-- Funcion que nos muestra la lista de las palabras del texto cargado


-- Funcion que nos muestra todas las palabras que no se encuentran en el diccionario.
nPalabras :: Resultado -> IO ()
nPalabras [] = putStr ""
nPalabras (x:xs) = do { putStr $ "\n" ++ x;
						nPalabras xs;
					  }

-- El diccionario en arbol.
 

-- Funcion que construye el arbol del diccionario.
dict :: IO Arbol
dict = do { putStr ("Cargando el diccionario.\n");
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
			  putStr "\nPalabra guardada en el Diccionario local.\n";
			}

-- Funcion que nos regresa la lista de las palabras nuevas en formato IO.
busquedaIO :: Resultado -> IO ()
busquedaIO [] = putStr "0"
busquedaIO (x:xs) = putStr (show (length (x:xs)))

-- Funcion que nos muestra un menu pequeño.
miniM :: IO Char
miniM = do { putStr "\n\n¿Que te gustaria hacer?\n";
			 putStr "a) Agregar una nueva palabra.\n";
			 putStr "b) Salir del programa.\n";
			 putStr "\nOpcion: ";
			 opcion <- getChar;
			 return opcion
		   }

-- Funcion que da el comportamiento de las opciones dadas en el menu.
opciones :: Char -> IO ()
opciones 'a' = nuevaP
opciones 'b' = putStr "\n\nBye bye...\n"
opciones _ = putStr "\n\nOpcion invalida.\n\n"

-- Funcion main
main :: IO ()
main = do { d <- dict;
				p <- cargaTxt;
				putStr "Las siguientes palabras no estan correctas o son nuevas.\n";
				nPalabras (busqueda p d);
					m <- miniM;
					opciones m;
					if m == 'b' then putStr "\nHasta pronto.\n" else main;
		  }