module Main ( main ) where

import System.IO
import Data.Char(toUpper)
import System.Posix.Files
import System.Environment (getArgs)


-- Funcion que lee por linea
parser :: [String] -> [[String]]

parser [] = [] 
parser (x:xs) = parserPrima x x 0 : parser xs

  
-- Funcion que me retorna lista de algo
parserPrima :: String -> String -> Int -> [String]

parserPrima "" _ _ = []
parserPrima (y:ys) xs end
    | y == ','  =  guardarPalabra (xs) (end) : parserPrima ys (drop (end + 1) xs) 0  
    | y == '\n' =  parserPrima ys (drop 1 xs) (end)
    | otherwise =  parserPrima ys xs (end+1)
    
  where 
    guardarPalabra (w:ws) end
      | (end == 0) && (w == ',') = ""
      | (end == 0) && (w /= ',') = []
      | otherwise = w : guardarPalabra ws (end - 1) 
    
 
-- Funcion que abre los archivos de entrada
abrirFile :: [String] -> IO ()

abrirFile args = do 
  fileEsp  <- readFile ((!!) args 0) 
  fileAtaq <- readFile ((!!) args 1) 
  equipo1  <- readFile ((!!) args 2) 
  equipo2  <- readFile ((!!) args 3) 
  print $ parser [fileEsp, fileAtaq, equipo1, equipo2]
  
-- Funcion principal 
main :: IO ()

main = do 
  args  <- getArgs
  if (length args) == 4 then
    abrirFile(args)
  else 
    print "Tiene que introducir 4 nombres de archivos"
  
