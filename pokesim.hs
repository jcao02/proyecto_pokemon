module Main ( main ) where

import Pokemon
import System.IO
import Data.Maybe
import Data.Char(toUpper)
import Data.List.Split
import System.Posix.Files
import System.Environment (getArgs)
import qualified Data.Map as Dic


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
listaAtaque = map crear  
  where
    crear [c0,c1,c2,c3,c4] = 
      Ataque c0 (read c1 :: Tipo) (read c2  :: Bool) 
		(read c3  :: Int) (read c4  :: Int) 

		
-- Funcion que crea una lista de monstruo por equipo participante	   
listaMonstruo :: [[String]] -> [Especie] -> [Ataque] -> [Monstruo]   
listaMonstruo [] esp atq = []
listaMonstruo (x:xs) esp atq = case crear x of
  Nothing -> listaMonstruo xs esp atq
  Just x  -> x : listaMonstruo xs esp atq
  where
    crear [c0,c1,c2,c3,c4,c5,c6] = Monstruo esps c1 (read c2  :: Int) 0 atqs 35 255
	  where
	    esps = Dic.lookup (read c0 :: Int) esp
	    atqs  = catMaybes (map (\ c -> Dic.lookup c atq) [c3, c4, c5, c6])
	    

	    
-- Diccionario de Especies 
diccEspecie :: [Especie] -> Dic.Map Int Especie
diccEspecie  = foldl (\ mapa esp -> Dic.insert (numero esp) esp mapa) Dic.empty 


-- Diccionario de Ataques
diccAtaque :: [Ataque] -> Dic.Map String Ataque
diccAtaque  = foldl (\ mapa ata -> Dic.insert (nombreAtaq ata) ata  mapa) Dic.empty 


-- Funcion que parsea los archivos  
parser :: String -> [[String]]
parser x = map (splitOn ",") (lines x)
  
 
-- Funcion que abre los archivos de entrada
abrirFile :: [String] -> IO ()
abrirFile [fileEspS,fileAtaqS,equipo1S,equipo2S] = do 
  fileEsp  <- readFile fileEspS 
  fileAtaq <- readFile fileAtaqS
  equipo1  <- readFile equipo1S
  equipo2  <- readFile equipo2S
  
  -- Parseo los archivos de las especies, ataques y equipos
  let [espe, atque, equip1 , equip2] = map parser [fileEsp, fileAtaq, equipo1, equipo2]
  
  -- Se define un diccionario de Especies
  let esp = diccEspecie (listaEspecie espe)
  
  -- Se define un diccionario de ataques
  let atq = diccAtaque (listaAtaque atque)
  
  -- Se Crean los equipos participantes
  let equipo1 = listaMonstruo (equip1) esp atq
  let equipo2 = listaMonstruo (equip2) esp atq
  print "Bienvenido a una Nueva Batalla Pokemon"
  
  
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 then abrirFile(args) else print "Tiene que introducir 4 nombres de archivos"
  
