module Main ( main ) where

import Parseo 
import System.IO
import Pokemon
import Data.Char
import Data.List.Split
import Batalla
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
  let pesp = diccEspecie (listaEspecie espe)

  -- Se asignan las preevoluciones en el diccionario
  let esp = setPreevolucion pesp
  
  -- Se define un diccionario de ataques
  let atq = diccAtaque (listaAtaque atque)
  
  -- Se Crean los equipos participantes
  let equipo1 = listaMonstruo (equip1) esp atq
  let equipo2 = listaMonstruo (equip2) esp atq
  
  print "BIENVENIDO A UNA BATALLA POKEMON"
  turno (Entrenador 1 0 equipo1) (Entrenador 2 0 equipo2)
  putStr ""
  
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 
    then do
      abrirFile(args)
    else 
      print "Tiene que introducir 4 nombres de archivos"
  
