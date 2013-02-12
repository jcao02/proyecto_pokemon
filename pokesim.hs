module Main ( main ) where

import Pokemon
import System.IO
import Data.Char(toUpper)
import Data.List.Split
import System.Posix.Files
import System.Environment (getArgs)


-- Funcion que crea una lista de especies ---
listaEspecie :: [[String]] -> [Especie]   
listaEspecie [] = []
listaEspecie (x:xs) = crear x : listaEspecie xs 
  where
    crear [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11] = Especie (read c0  :: Int) c1 
	  [read c2 :: Tipo, read c3 :: Tipo] (read c4  :: Int) (read c5 :: Int) 
	  (read c6  :: Int) (read c7 :: Int) (read c8  :: Int) (read c9 :: Int) c10 c11
	-- La lista no es de string sino de tipo TIPO

	
-- Funcion que crea una lista de ataques    
listaAtaque :: [[String]] -> [Ataque]   
listaAtaque [] = []
listaAtaque (x:xs) = crear x : listaAtaque xs 
  where
    crear [c0,c1,c2,c3,c4] = 
      Ataque c0 (read c1 :: Tipo) (read c2  :: Bool) 
		(read c3  :: Int) (read c4  :: Int) 

		
-- Funcion que crea una lista de monstruo		   
listaMonstruo :: [[String]] -> [Especie] -> [Ataque] -> [Monstruo]   
listaMonstruo [] esp atq = []
listaMonstruo (x:xs) esp atq = crear x : listaMonstruo xs esp atq
  where
    crear [c0,c1,c2,c3,c4,c5,c6] = Monstruo (getEspecie esp (read c0 :: Int)) c1 (read c2  :: Int) 0 
	  [getAtaque atq c3, getAtaque atq c4, getAtaque atq c5, getAtaque atq c6] 35 255		
		
		
-- Funcion que me devuelve una Especie	
getEspecie ::[Especie] -> Int -> Especie
getEspecie esp num = (!!) (filter (\x -> numero x == num) esp) 0 
  
  
-- Funcion que me devuelve un Ataque	
getAtaque ::[Ataque] -> String -> Ataque
getAtaque atq num = (!!) (filter (\x -> nombreAtaq x == num) atq) 0 

		   
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
  let parseado = parser [fileEsp, fileAtaq, equipo1, equipo2]
  let esp = listaEspecie ((!!) parseado 0)
  let atq = listaAtaque ((!!) parseado 1)
  let equipo1 = listaMonstruo ((!!) parseado 2) esp atq
  let equipo2 = listaMonstruo ((!!) parseado 3) esp atq
  print esp
  
  
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 then abrirFile(args) else print "Tiene que introducir 4 nombres de archivos"
  
