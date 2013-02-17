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
listaEspecie = map crear  
  where
    crear [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11] = Especie { numero      = (read c0  :: Int) 
                                                            , nombreEsp   = c1
                                                            , tipoElem    = tipo $ listaMaybes [c2,c3]
                                                            , hp          = read c4 :: Int
                                                            , ataque      = read c5 :: Int
                                                            , defensa     = read c6 :: Int
                                                            , ataqueEsp   = read c7 :: Int
                                                            , defensaEsp  = read c8 :: Int
                                                            , velocidad   = read c9 :: Int
                                                            , prevolucion = c10
                                                            , evolucion   = c11
                                                            }
    listaMaybes xs = catMaybes ( map (\x -> if x == "" then Nothing else Just x) xs ) 
    tipo []     = []
    tipo (x:xs) = (read x :: Tipo) : tipo xs 
	-- La lista no es de string sino de tipo TIPO

	
-- Funcion que crea una lista de ataques    
listaAtaque :: [[String]] -> [Ataque]   
listaAtaque = map crear  
  where
    crear [c0,c1,c2,c3,c4] = Ataque { nombreAtaq = c0 
                                    , tipo      = (read c1 :: Tipo) 
                                    , fisico    = (read c2  :: Bool) 
                                    , pps       = (read c3  :: Int) 
                                    , pow       = (read c4  :: Int) 
                                    }

		
-- Funcion que crea una lista de monstruo por equipo participante	   
listaMonstruo :: [[String]] -> Dic.Map Int Especie -> Dic.Map String Ataque -> [Monstruo]   
listaMonstruo [] _ _ = []
listaMonstruo xs esp atq = map crear xs
  where
    crear :: [String] -> Monstruo
    crear [c0, c1, c2, c3, c4, c5, c6] = Monstruo { especie     = esps
					                                        , sobreNombre = c1
                                                  , nivel       = read c2 :: Int
                                                  , hpAct       = 0
                                                  , ataques     = atqs
                                                  , individual  = 31
                                                  , esfuerzo    = 255
                                                  }
      
	    where
	      esps = fromJust $ Dic.lookup (read c0 :: Int) esp
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

  print "Especies.."
  print $ Dic.toList esp 

  print "Ataques..."
  print $ Dic.toList atq
  
  print "Equipo 1..."
  print equipo1

  print "Equipo 2..."
  print equipo2
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 then abrirFile(args) else print "Tiene que introducir 4 nombres de archivos"
  
