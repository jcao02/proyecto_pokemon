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
  turno (Entrenador 1 0 equipo1 (length equipo1)) (Entrenador 2 0 equipo2 (length equipo2))
  putStr ""
  
    
-- funcion que muestra la informacion del monstruoActual del atacante o el defensor    
mostrarInfo :: Entrenador -> IO ()
mostrarInfo ent = infoPokemon (especie (getActual ent))
  where
    infoPokemon esp = do 
      putStrLn "\n\nINFORMACION DE CATALOGO"
      putStr   $ "Numero          : "
      print    $ numero esp
      putStrLn $ "Nombre          : " ++ nombreEsp esp 
      putStr   $ "Tipos           : " 
      print    $  tipoElem esp
      putStr   $ "HP              : "
      print    $ hp esp
      putStr   $ "Ataque          : "
      print    $ ataque esp
      putStr   $ "Defensa         : "
      print    $ defensa esp
      putStr   $ "Ataque Especial : "
      print    $ ataqueEsp esp
      putStr   $ "Defensa Especial: "
      print    $ defensaEsp esp
      putStr   $ "Velocidad       : "
      print    $ velocidad esp
      putStrLn $ "Pre-Evolucion   : " ++ prevolucion esp ++ "\n\n"
      
    
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
  putStrLn "\n\nLISTA DE POKEMONES ACTIVOS"
  printPokemon 0
  where 
      printPokemon num = do
        if num < length poke 
           then do  
            putStrLn $ "Introduzca 'cambiar "++ show num ++"' para escoger al pokemon " ++ sobreNombre ((!!) poke num)
            printPokemon (num + 1)
           else 
            putStrLn ""  
    
-- Funcion que imprime la informacion de los ataques				
imprimirAtaques :: [String] -> [Int] -> IO ()
imprimirAtaques atqA ppsA = do
  putStrLn "\n\nLISTA DE ATAQUES"
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
  putStr "ENTRENADOR "
  print $ posicion ent1
  putStrLn "Introduzca un Comando"
  comando <- getLine
  if comando == "\n" then 
    getComando ent1 ent2
  else do
    let parseoComando = map toUpper ((!!) (words comando) 0)
    let opcion = read $ words comando !! 1 :: Int
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
	 ATACAR    -> if puedeAtacar ent1 opcion then return (com , read (words comando !! 1) :: Int )
				       else do
					 putStrLn "No hay suficientes PPs para atacar, intente de nuevo"
					 getComando ent1 ent2         
	 CAMBIAR   -> if (estaConciente $ (pokemones ent1) !! opcion) && (opcion - 1 /= (actual ent1))  then return (com , read (words comando !! 1) :: Int )
                                       else do
                                         putStrLn "El pokemon que eligio no es valido. Intente de nuevo"
                                         getComando ent1 ent2
      
           
    
	 
-- Funcion que realiza las acciones que restan turno
turno :: Entrenador -> Entrenador -> IO ()
turno ent1 ent2 = do
  comandos <- getAcciones ent1 ent2 
  case comandos of 
       ((RENDIRSE,-1),(RENDIRSE,-1))   -> jugadaRendirse 0
       ( _ ,(RENDIRSE,-1))             -> jugadaRendirse (posicion ent2)
       ((RENDIRSE,-1), _ )             -> jugadaRendirse (posicion ent1)
       ((CAMBIAR,num1), (ATACAR,num2)) -> do -- Cambio del pokemon del 1er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent1)
					     putStr $ "pokemon antes del cambio " ++ sobreNombre (getActual ent1)
					     let aux = cambiarPokemon ent1 num1
					     let ent1 = aux
					     putStr $ "pokemon despues del cambio " ++ sobreNombre (getActual ent1)
					     
					     -- Ataque del 2do Entrenador
					     putStrLn $ "/nRealizando ataque al pokemon " ++ sobreNombre (getActual ent1)
					     putStrLn $ "hp del atacado antes del ataque " 
					     print (hpAct (getActual ent1))
					     putStrLn $ "pps del atacante antes del ataque "
					     let aux = num2-1 
					     print (pps ((!!) (ataques (getActual ent2)) aux))
					     print $ (nombreAtaq (ataques (getActual ent2) !! aux))
					     (ent2, ent1) <- atacar ent2 ent1 num2 
					     putStrLn $ "hp del atacado despues del ataque "
					     print (hpAct (getActual ent1))
					     putStrLn $ "pps del atacante despues del ataque "
					     print (pps ((!!) (ataques (getActual ent2)) aux))
       ((ATACAR,num1), (CAMBIAR,num2)) -> do -- Ataque del 1er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent2)
					     let ent2 = cambiarPokemon ent2 num2
					     putStrLn $ "Realizando ataque al pokemon " ++ sobreNombre (getActual ent2)
					     (ent1, ent2) <- atacar ent1 ent2 num1 
					     putStr ""
       ((CAMBIAR,num1),(CAMBIAR,num2)) -> do -- Cambio del pokemon del 1er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent1)
					     let ent1 = cambiarPokemon ent1 num1
					     -- Cambio del pokemon del 2er Entrenador
					     putStr "Realizando cambio de pokemon para el Entrenador "
					     print (posicion ent2)
					     let ent2 = cambiarPokemon ent2 num2      
                                             putStr ""
       ((ATACAR,num1) , (ATACAR,num2)) -> do -- Ataque entre entrenadores
					     let (nEnt1,nEnt2) = chequearVelocidad ent1 ent2
					     -- Ataque del 1er Entrenador
					     putStr $ "Realizando ataque al pokemon " ++ sobreNombre (getActual nEnt2)
					     (ent1, ent2) <- atacar nEnt1 nEnt2 num1
					     let (nEnt1, nEnt2) = (ent1,ent2)
					     -- Ataque del 2do Entrenador
					     putStr $ "Realizando ataque al pokemon " ++ sobreNombre (getActual ent1)
					     (ent2, ent1) <- atacar nEnt2 nEnt1 num2
					     putStr ""
       otherwise                       -> do putStrLn "Introdujeron comandos invalidos"

                                              
  nEnt1 <- chequearVivos ent1 1
  nEnt2 <- chequearVivos ent2 2
  print $ activos nEnt1
  print $ activos nEnt2
  if perdio ent1 
      then putStrLn "Ganador: Entrenador 2"
  else if perdio ent2
      then putStrLn "Ganador: Entrenador 1"
  else
    turno nEnt1 nEnt2
    
-- Funcion que imprime la informacion del rendido  
jugadaRendirse :: Int -> IO () 
jugadaRendirse num = 
  case num of 
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
  
