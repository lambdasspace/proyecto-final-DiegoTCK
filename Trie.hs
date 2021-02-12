module Trie where

-- Inicio del proyecto.

{- Definicion del arbol para el Trie
-- En si el arbol es la estructura que usare para la verificacion de las palabras. 
-}
data Arbol = Raiz [Arbol] | Flor Char [Arbol] | Hoja deriving(Eq, Show)



{- Seccion de funciones auxiliares -}

-- Funcion auxiliar palabraNarbol transforma una lista de Char a un Arbol
palabraNarbol :: [Char] -> Arbol
palabraNarbol [] = Hoja
palabraNarbol (x:xs) = Flor x [palabraNarbol xs]

