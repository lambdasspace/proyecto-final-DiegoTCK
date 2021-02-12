module Trie where

-- Inicio del proyecto.

-- Definicion del arbol para el Trie
data Arbol = Raiz [Arbol] | Flor Char [Arbol] | Hoja deriving(Eq, Show)