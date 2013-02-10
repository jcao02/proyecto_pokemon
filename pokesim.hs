module Main ( main ) where

import System.IO
import Data.Char(toUpper)
import System.Posix.Files
import System.Environment (getArgs)



-- Funcion que abre los archivos de entrada
abrirFile :: [String] -> IO ()

abrirFile args = do 
  fileEsp  <- readFile ((!!) args 0) 
  fileAtaq <- readFile ((!!) args 1) 
  equipo1  <- readFile ((!!) args 2) 
  equipo2  <- readFile ((!!) args 3) 
  putStr (fileEsp)
  
-- Funcion principal 
main :: IO ()

main = do 
  args  <- getArgs
  if (length args) == 4 then
    abrirFile(args)
  else 
    print "Tiene que introducir 4 nombres de archivos"
  
