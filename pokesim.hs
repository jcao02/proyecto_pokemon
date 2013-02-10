module Main ( main ) where

import Pokemon
import System.IO
import Data.Char(toUpper)
import Data.List.Split
import System.Posix.Files
import System.Environment (getArgs)


-- Funcion que parsea los archivos    
parser :: [String] -> [[[String]]]

parser [] = []
parser (x:xs) = separarComas (splitOn "\n" x) : parser xs
  where 
    separarComas [] = []
    separarComas (x:xs) = splitOn "," x : separarComas xs 
   
 
-- Funcion que abre los archivos de entrada
abrirFile :: [String] -> IO ()

abrirFile args = do 
  fileEsp  <- readFile ((!!) args 0) 
  fileAtaq <- readFile ((!!) args 1) 
  equipo1  <- readFile ((!!) args 2) 
  equipo2  <- readFile ((!!) args 3) 
  let hola = parser [fileEsp, fileAtaq, equipo1, equipo2]
  print "Bienvenido a una partida Pokemon"
  
  
-- Funcion principal 
main :: IO ()

main = do 
  args  <- getArgs
  if (length args) == 4 then abrirFile(args) else print "Tiene que introducir 4 nombres de archivos"
  
