module Main ( main ) where

import Parseo 
import System.IO
import Pokemon
import Data.Char
import Data.List.Split
import Batalla
import System.Environment (getArgs)
import qualified Data.Map as Dic



-- Declaracion del tipo comando
data Comando =	ATACAR  
  | CAMBIAR 
  | INFO 
  | AYUDA
  | RENDIRSE  
  deriving (Show,Read,Eq) 
  
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
  print "BIENVENIDO A UNA BATALLA POKEMON"
  turno (Entrenador 1 0 equipo1 False) (Entrenador 2 0 equipo2 False)
  putStr ""
  
    
-- funcion que muestra la informacion del monstruoActual del atacante o el defensor    
mostrarInfo :: Entrenador -> IO ()
mostrarInfo ent = infoPokemon (getActual ent)
  where
    infoPokemon mons = do 
      putStrLn $ "SobreNombre: " ++ sobreNombre mons
      putStrLn $ "Especie    : " ++ nombreEsp (especie mons)
      putStr   $ "Nivel      : " 
      print    $  nivel mons
      putStr   $ "HP Actual  : "
      print    $ hpAct mons
      putStr   $ "Ataques    : "
      print    listaAta
      where 
	listaAta = map nombreAtaq (ataques mons)
      
    
-- Funcion que muestra la ayuda sobre comandos
mostrarAyuda :: Entrenador -> IO()
mostrarAyuda ent = do 
  imprimirAtaques ataquesAyuda ppsAyuda 
  imprimirPokemon monstruoAyuda
    where
      ataquesAyuda  = map nombreAtaq (ataques (getActual ent))
      ppsAyuda      = map pps (ataques (getActual ent))
      monstruoAyuda = pokemones ent
    
    
-- Funcion que imprime la informacion de los monstruos de un entrenador
imprimirPokemon :: [Monstruo] -> IO ()     
imprimirPokemon poke = do    
  putStrLn "LISTA DE POKEMONES ACTIVOS"
  putStrLn $ "Introduzca 'cambiar 1' para escoger al pokemon " ++ sobreNombre ((!!) poke 0)
  putStrLn $ "Introduzca 'cambiar 2 'para escoger al pokemon " ++ sobreNombre ((!!) poke 1)
  putStrLn $ "Introduzca 'cambiar 3' para escoger al pokemon " ++ sobreNombre ((!!) poke 2)
  putStrLn $ "Introduzca 'cambiar 4' para escoger al pokemon " ++ sobreNombre ((!!) poke 3)
    
    
-- Funcion que imprime la informacion de los ataques				
imprimirAtaques :: [String] -> [Int] -> IO ()
imprimirAtaques atqA ppsA = do
  putStr $ "Introduzca 'atacar 1' para atacar con "++ ((!!) atqA 0) ++ " con pps = " 
  print ((!!) ppsA 0)  
  putStr $ "Introduzca 'atacar 2' para atacar con "++ ((!!) atqA 1) ++ " con pps = " 
  print  ((!!) ppsA 1) 
  putStr $ "Introduzca 'atacar 3' para atacar con "++ ((!!) atqA 2) ++ " con pps = " 
  print ((!!) ppsA 2) 
  putStr $ "Introduzca 'atacar 4' para atacar con "++ ((!!) atqA 3) ++ " con pps = " 
  print ((!!) ppsA 3)   
  putStrLn "RECUERDE: los ataques con pps menor a 1 no podran ser utilizados\n"


-- Funcion que devuelve una tupla de acciones que "restan turno"  
getAcciones :: Entrenador -> Entrenador -> IO ((Comando,Int),(Comando,Int))
getAcciones ent1 ent2 = do
  comand1 <- getComando ent1 ent2 
  comand2 <- getComando ent2 ent1 
  return (comand1,comand2)
  
  
-- Funcion que obtiene los comandos por pantalla  
getComando :: Entrenador -> Entrenador ->  IO ((Comando,Int))
getComando ent1 ent2 = do
  putStr "\nEntrenador "
  print $ posicion ent1
  putStrLn " introduzca un Comando"
  comando <- getLine
  if comando == "\n" then 
    getComando ent1 ent2
  else do
    let parseoComando = map toUpper ((!!) (words comando) 0)
    let com = read parseoComando :: Comando
    case com of 
	 AYUDA     -> do mostrarAyuda ent1
			 getComando ent1 ent2
	 INFO      -> do if ((!!) (words comando) 1) == "yo" then do
			    mostrarInfo ent1
			    getComando ent1 ent2
			 else if ((!!) (words comando) 1) == "rival" then do
			    mostrarInfo ent2
			    getComando ent1 ent2
			    else do
			      putStrLn "Introdujo un comando invalido"
			      getComando ent1 ent2
	 RENDIRSE  -> return (com , -1)
	 ATACAR x  -> if puedeAtacar $ ent1 x then return (com , read ((!!) (words comando) 1) :: Int )
				       else do
					 putStrLn "No hay suficientes PPs para atacar, intente de nuevo"
					 getComando ent1 ent2
	 otherwise -> return (com , read ((!!) (words comando) 1) :: Int )
    
	 
-- Funcion que realiza las acciones que restan turno
turno :: Entrenador -> Entrenador -> IO ()
turno nEnt1 nEnt2 = do
  let ent1 = chequearVivos nEnt1
  let ent2 = chequearVivos nEnt2
  comandos <- getAcciones ent1 ent2 
  case comandos of 
       ((RENDIRSE,-1),(RENDIRSE,-1))   -> jugadaRendirse 0
       ( _ ,(RENDIRSE,-1))             -> jugadaRendirse (posicion ent2)
       ((RENDIRSE,-1), _ )             -> jugadaRendirse (posicion ent1)
       ((CAMBIAR,num1), (ATACAR,num2)) -> do -- Realizar cambio del pokemon del 1er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent1)
					     putStr $ "pokemon antes del cambio " ++ sobreNombre (getActual ent1)
					     let aux = cambiarPokemon ent1 num1
					     let ent1 = aux
					     -- Realizar ataque del 2do Entrenador
					     putStrLn $ "/nRealizando ataque al pokemon " ++ sobreNombre (getActual ent1)
					     let mons = chequearVelocidad ent2 ent1
					     --atacar (snd mons) (fst mons) 
					     -- CAMBIO DE TURNO 
					     turno ent1 ent2 
       ((ATACAR,num1), (CAMBIAR,num2)) -> do -- Realizar ataque del 1er Entrenador
					     putStrLn $ "Realizando ataque al pokemon " ++ sobreNombre (getActual ent2)
					     let mons = chequearVelocidad ent1 ent2 
					     --atacar (fst mons) (snd mons) 
					     -- Realizar cambio del pokemon del 2do Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent2)
					     let ent2 = cambiarPokemon ent2 num2
					     -- CAMBIO DE TURNO 
					     turno ent1 ent2 
       ((CAMBIAR,num1),(CAMBIAR,num2)) -> do -- Realizar cambio del pokemon del 1er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent1)
					     let ent1 = cambiarPokemon ent1 num1
					     -- Realizar cambio del pokemon del 2er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent2)
					     let ent2 = cambiarPokemon ent2 num2
					     -- CAMBIO DE TURNO 
					     turno ent1 ent2 
       ((ATACAR,num1) , (ATACAR,num2)) -> do -- Realizar ataque entre entrenadores
					     putStr $ "Realizando ataque al pokemon " ++ sobreNombre (getActual ent1)
					     let mons = chequearVelocidad ent1 ent2  
					     --atacar (fst mons) (snd mons) 
					     putStr $ "Realizando ataque al pokemon " ++ sobreNombre (getActual ent2)
					     --atacar (snd mons) (fst mons)
       otherwise                       -> do print "Introdujeron comandos invalidos"
					     -- NUEVO TURNO 
					     turno ent1 ent2  
    
    
-- Funcion que imprime la informacion del rendido  
jugadaRendirse :: Int -> IO () 
jugadaRendirse num = case num of 
			  0         -> do putStrLn "Ambos jugadores se han rendido"
					  putStrLn "\nGAME OVER"
			  1         -> do putStrLn "Entrenador 1 se ha rendido"
					  putStrLn "Entrenador 2 GANADOR"
			  2         -> do putStrLn "Entrenador 2 se ha rendido"
					  putStrLn "Entrenador 1 GANADOR"
			  otherwise -> putStrLn "Nadie se ha rendido"
  
  
-- Funcion principal 
main :: IO ()
main = do 
  args  <- getArgs
  if (length args) == 4 then do
    abrirFile(args)
  else 
    print "Tiene que introducir 4 nombres de archivos"
  
