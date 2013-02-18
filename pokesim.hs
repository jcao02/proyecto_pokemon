module Main ( main ) where

import Parseo 
import System.IO
import System.Environment (getArgs)
import qualified Data.Map as Dic


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
  turno (Entrenador 0 equipo1 False) (Entrenador 0 equipo2 False) 
  print "Bienvenido a una Nueva Batalla Pokemon"

--   print "Especies.."
--   print $ Dic.toList esp 
-- 
--   print "Ataques..."
--   print $ Dic.toList atq
--   
--   print "Equipo 1..."
--   print equipo1
-- 
--   print "Equipo 2..."
--   print equipo2

-- Funcion que determina el tipo de comando
verificarUno :: [String] -> Entrenador -> Entrenador -> IO ()
verificarUno l ent1 ent2 = do 
  if primer == "info[yo]" then do mostrarInfo ent1  putStrLn "Informacion de su pokemon" 
			  else if primer == "info[rival]" then do mostrarInfo ent2  putStrLn "Informacion del pokemon adversario" else if primer == "ayuda"       then do mostrarAyuda ent1 putStrLn "Ayuda" 
			  else if primer == "rendirse"    then rendirse ent1        putStrLn "El entrenador se ha rendido" 
			  else putStrLn "Ha introducido un comando invalido"
	
  where 
    primer = (!!) l 0

-- Funcion que determina el tipo de comando    
verificarDos :: [String] -> Entrenador -> Entrenador -> IO ()
verificarDos l ent1 ent2 = do 
  if primer == "atacar" then do atacar mons1 mons2 num putStrLn "Realizando el ataque seleccionado..." 
			else if primer == "cambiar" then do cambiarPokemon ent1 num putStrLn "Realizando el cambio seleccionado..." else putStrLn "Ha introducido un comando invalido"
    
  where 
    primer = (!!) l 0
    num    = (!!) l 1
    mons1  = (!!) (pokemones ent1) (actual ent1)
    mons2  = (!!) (pokemones ent2) (actual ent2)
    
-- funcion que muestra la informacion del monstruoActual del atacante o el defensor    
mostrarInfo :: Entrenador -> IO ()
mostrarInfo ent = print monstruoActual
  where 
    monstruoActual = (!!) (pokemones ent) (actual ent)
    
-- funcion que muestra la ayuda    
    
turno :: Entrenador -> Entrenador -> IO ()
turno ent1 ent2 = do 
  putStrLn "Introduzca un Comando"
  comando    <- getLine
  let cadena =  words comando
  print cadena
  if length cadena == 0 then putStrLn "Tiene que introducir algun comando" 
			else if length cadena   == 1 then verificarUno cadena ent1 
			else if length cadena == 2 then verificarDos cadena ent1 ent2 
			else print "Ha introducido un comando invalido"    

    
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 then do
    abrirFile(args)
  else 
    print "Tiene que introducir 4 nombres de archivos"
  
