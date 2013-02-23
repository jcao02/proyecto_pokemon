module Batalla where

import Data.Char
import Pokemon

-- Declaracion del tipo comando
data Comando =	ATACAR  
  | CAMBIAR 
  | INFO 
  | AYUDA
  | RENDIRSE  
  deriving (Show,Read,Eq) 

-- Funciones...
-- Obtiene el pokemon actual de un entrenador
getActual :: Entrenador -> Monstruo
getActual ent = pokemones ent !! actual ent


-- Determina si el Monstruo fue derrotado
derrotado :: Monstruo -> Bool
derrotado m = (hpAct m) <= 0


-- Determina si un entrenador puede atacar con su pokemon actual
puedeAtacar :: Entrenador -> Int -> Bool
puedeAtacar ent atq = pps (ataques (getActual ent) !! (atq - 1)) > 0 

  

-- Retorna en una tupla de monstruos el monstruo mas rapido de primero                                   
chequearVelocidad :: Entrenador -> Entrenador -> (Entrenador,Entrenador) 
chequearVelocidad ent1 ent2
  | v1 > v2   = (ent1,ent2)
  | v1 < v2   = (ent2,ent1)
  | otherwise = (ent1,ent2)
  
  where 
    v1    = estadistica 31 base1 255 nvl1
    v2    = estadistica 31 base2 255 nvl2
    base1 = velocidad (especie (getActual ent1))
    base2 = velocidad (especie (getActual ent2))
    nvl1  = nivel (getActual ent1)
    nvl2  = nivel (getActual ent2)
  
-- Funcion que determine si un entrenador perdio 
perdio :: Entrenador -> Bool
perdio ent = all (derrotado) (pokemones ent)

-- Actualiza la lista de pokemons con los vivos
chequearVivos :: Entrenador -> Int -> IO Entrenador 
chequearVivos ent num = do
  if derrotado $ getActual ent 
    then do 
      putStr   $ "Entrenador " ++ (show num)
      putStrLn $ " su pokemon no puede continuar, elija uno nuevo" 
      putStrLn ""
      imprimirPokemon $ pokemones ent
      verificarComando
    
    else 
      return ent

  where
    verificarComando = do
      input <- getLine
      let parseoComando = map toUpper (words input !! 0)
      let opcion = read $ words input !! 1 :: Int
      let accion = read parseoComando :: Comando
      case accion of
        CAMBIAR -> do
          nactual <- verificarCambio ent opcion
          let entaux = ent { actual = nactual }
          mostrarCambio num entaux
          return $ cambiar ent nactual

        otherwise -> do
          putStrLn "Debe elegir un pokemon nuevo para continuar."
          verificarComando
      

-- Funcion que cambia un pokemon luego de verificar que sea valido
cambiar :: Entrenador -> Int -> Entrenador
cambiar ent n = ent { actual = n }



-- Funcion que verifica que el cambio solicitado sea valido
verificarCambio :: Entrenador ->Int -> IO Int
verificarCambio ent n = do
  let tam = length (pokemones ent)
  case tam of
    1         -> do 
      putStrLn "No puede realizar ningun cambio"
      return $ -1
    otherwise -> do
      if (n > tam) || (n <= 0) 
        then do
          putStrLn "Esta seleccionando un pokemon invalido. Intente de nuevo"
          return $ -1
        else if derrotado $ pokemones ent !! (n - 1)
          then do
            putStrLn "Esta seleccionando un pokemon invalido. Intente de nuevo"
            return $ -1 
          else if (n - 1) == actual ent 
            then do 
              putStrLn "Elija un pokemon distinto al actual"
              return $ -1
        else
          return $ n - 1
 
-- Ataque
atacar :: Entrenador -> Entrenador -> Int -> IO (Entrenador, Entrenador)
atacar ent1 ent2 atq = do
  mostrarAtaque ent1 (ataques (getActual ent1) !! (atq - 1))
  return $ (ent1 { pokemones = npokemones (pokemones ent1) },  ent2 { pokemones = defpokemones (pokemones ent2)})
  where
    -- Funcion que actualiza lista de pokemones del entrenador que ataco
    npokemones = map (\x -> if x == getActual ent1 then x { ataques = nataques (ataques x)} else x) 

    -- Funcion que actualiza lista de pokemones del entrenador defensor
    defpokemones = map (\x -> if x == getActual ent2 then x { hpAct = (hpAct x) - dano } else x) 

    -- Funcion que actualiza el ataque utilizado con el nuevo PPs de la lista de ataques el Monstruo Ofensivo
    nataques = map (\x -> if x == ataques (getActual ent1) !! (atq - 1) then x { pps = (pps x) -1 } else x)

    -- Funcion que retorna la nueva hp dado el daño inflijido por el Pokemon Ofensivo al Defensivo 
    dano = damage lvl power atk dfse modi
      where
        lvl = nivel (getActual ent1)
        power = pow $ (ataques (getActual ent1)) !! (atq - 1)
          -- Si es fisico el ataque, se usa el atributo ataque, de lo contrario se usa el Ataque especial
        atk
          | fisico $  ataques (getActual ent1) !! (atq - 1) = 
            estadistica 31 (ataque (especie (getActual ent1))) 255 (nivel (getActual ent1))
          | otherwise                        = 
            estadistica 31 (ataqueEsp (especie (getActual ent1))) 255 (nivel (getActual ent1))
                  -- Si es fisico el ataque, se usa el atributo defensa, de lo conrario se usa la defensa especial
        dfse 
          | fisico $  ataques (getActual ent1) !! (atq - 1) = 
            estadistica 31 (defensa (especie (getActual ent2))) 255 (nivel (getActual ent2)) 
          | otherwise                        = 
            estadistica 31 (defensaEsp (especie (getActual ent2))) 255 (nivel (getActual ent2))
                
        -- Modificador de daño
        modi = modDano (especie (getActual ent1)) ((ataques (getActual ent1)) !! (atq-1)) (especie (getActual ent2)) 
    -- Funcion que imprime la accion de Ataque
    mostrarAtaque :: Entrenador -> Ataque -> IO ()
    mostrarAtaque ent atk = do
      putStr $ "\n"
        ++ sobreNombre (getActual ent) 
        ++ " ataca con "
        ++ nombreAtaq atk ++ "."
      if modi >= 2 
        then do
          putStrLn " Es super efectivo\n"
        else if modi == 1
          then do
            putStrLn ""
          else do
            putStrLn " No es muy efectivo\n"
      where
        modi = modDano (especie (getActual ent1)) ((ataques (getActual ent1)) !! (atq-1)) (especie (getActual ent2)) 

-- Funcion que verifica que un ataque sea valido
verificarAtaque :: Entrenador -> Int -> IO Int
verificarAtaque ent n = do  
  let tam = length $ ataques $ getActual ent
  if (n > tam) || (n <= 0)
    then do
      putStrLn "Ataque invalido"
      return $ -1 
    else if not $ puedeAtacar ent n 
      then do
        putStrLn "No tiene suficientes PPs para atacar"
        return $ -1
      else
        return n 

-- Funcion que muestra la informacion del monstruoActual del atacante o el defensor    
mostrarInfo :: Entrenador -> IO ()
mostrarInfo ent = do
  putStrLn "INFORMACION POKEMON"
  putStrLn $ show (especie (getActual ent))
      
    
-- Funcion que muestra la ayuda sobre comandos
mostrarAyuda :: Entrenador -> IO()
mostrarAyuda ent = do 
  imprimirAtaques ataquesAyuda ppsAyuda tipoAyuda
  imprimirPokemon monstruoAyuda
    where
      ataquesAyuda  = map nombreAtaq (ataques (getActual ent))
      ppsAyuda      = map pps (ataques (getActual ent))
      tipoAyuda     = map tipo (ataques (getActual ent))
      monstruoAyuda = pokemones ent
    
    
-- Funcion que imprime la informacion de los monstruos de un entrenador
imprimirPokemon :: [Monstruo] -> IO ()     
imprimirPokemon poke = do    
  putStrLn "\nLISTA DE POKEMONES"
  printPokemon 0
  where 
      printPokemon num = do
        if num < length poke 
          then do  
            putStr   $ "cambiar "++ show (num + 1)
            putStr   $ " -> " ++ sobreNombre ((!!) poke num)
            if derrotado $ poke !! num 
              then do
                putStrLn $ " (Inconciente)"
                printPokemon (num + 1)
              else do 
                putStrLn $ ""
                printPokemon (num + 1)
          else 
            putStrLn ""  
  
-- Funcion que imprime la informacion de los ataques				
imprimirAtaques :: [String] -> [Int] -> [Tipo] -> IO ()
imprimirAtaques atqA ppsA tipo = do
  putStrLn "\nLISTA DE ATAQUES"
  printAtaques 0
  where 
      printAtaques num = do
        if num < length atqA
           then do  
            putStr   $ "atacar " ++ show (num + 1) 
            putStr   $ " -> " ++ (atqA !! num)
            putStr   $ " (pps = " ++ show (ppsA !! num) 
            putStrLn $ ", tipo = " ++ show (tipo !! num) ++ ")"
            printAtaques (num + 1)
           else 
            putStrLn ""  


-- Funcion que devuelve una tupla de acciones que "restan turno"  
getAcciones :: Entrenador -> Entrenador -> IO ((Comando,Int),(Comando,Int))
getAcciones ent1 ent2 = do
  comand1 <- getComando ent1 ent2 
  comand2 <- getComando ent2 ent1 
  return (comand1,comand2)
  
  
-- Funcion que obtiene los comandos por pantalla  
getComando :: Entrenador -> Entrenador ->  IO ((Comando,Int))
getComando ent1 ent2 = do
  putStr   "\nEntrenador "
  putStrLn $ show $ posicion ent1
  putStrLn "Que desea hacer?"
  comando <- getLine
  putStrLn ""
  if comando == ""  -- Si no recibe nada
    then

    getComando ent1 ent2
    else do           -- Si recibe algo

      let parseoComando = map toUpper (words comando !! 0)  -- Obtiene la variable 
                                                            -- en mayuscula para 
                                                            -- leerla como comando
  
      let opcion = read $ words comando !! 1 :: Int         -- Parseo a Int de lo 
                                                            -- que acompaña al comando

      let com = read parseoComando :: Comando               -- Parseo del comando
  
      case com of                                           -- case para ver tipo de 
                                                            -- comando  
        AYUDA    -> do
          putStrLn "------------------------------"
          mostrarAyuda ent1
          putStrLn "------------------------------"
          getComando ent1 ent2
        INFO     -> do 
          if (words comando !! 1) == "yo"                   -- Caso que pida info yo
            then do
              putStrLn "------------------------------"
              mostrarInfo ent1
              putStrLn "------------------------------"
              getComando ent1 ent2
            else if (words comando !! 1) == "rival"           -- Caso que pida info rival
              then do
                putStrLn "------------------------------"
                mostrarInfo ent2
                putStrLn "------------------------------"
                getComando ent1 ent2
              else do
                putStrLn "Introdujo un comando invalido"
                getComando ent1 ent2

        RENDIRSE -> return (com , -1)
      
        ATACAR   -> do 

          opcion <- verificarAtaque ent1 opcion           
          if opcion == -1                                   -- Si la verificacion 
                                                          -- retorna -1 hay error
            then do
              getComando ent1 ent2
            else
              return (com , opcion)

        CAMBIAR  -> do
          opcion <- verificarCambio ent1 opcion
          if opcion == -1
            then do
              getComando ent1 ent2
            else
              return (com , opcion)
        
           
    
	 
-- Funcion que realiza las acciones que restan turno
turno :: Entrenador -> Entrenador -> IO ()
turno ent1 ent2 = do
  
  putStrLn "\n------------------------------"
  mostrarEstados ent1 ent2
  putStrLn "------------------------------"
  ent1 <- chequearVivos ent1 1      -- Reviso si el actual esta vivo y pido otro pokemon hasta que 
  ent2 <- chequearVivos ent2 2      -- valido (puede estar inconciente)
  comandos <- getAcciones ent1 ent2 

  case comandos of 
    ((RENDIRSE,_),(RENDIRSE,_))     -> jugadaRendirse 0 

    ( _ ,(RENDIRSE,_))              -> jugadaRendirse (posicion ent2)
 
    ((RENDIRSE,_), _ )              -> jugadaRendirse (posicion ent1)

    ((CAMBIAR,num1), (ATACAR,num2)) -> do   -- Entrenador 1 cambia, Entrenador 2 ataca

      let nEnt1 = cambiar ent1 num1
      mostrarCambio 1 nEnt1
      (ent2, ent1) <- atacar ent2 nEnt1 num2 
      verificaciones ent1 ent2 

    ((ATACAR,num1), (CAMBIAR,num2)) -> do   -- Entrenador 1 ataca, Entrenador 2 cambia

      let nEnt2 = cambiar ent2 num2
      mostrarCambio 2 nEnt2
      (ent1, ent2) <- atacar ent1 nEnt2 num1 
      verificaciones ent1 ent2

    ((CAMBIAR,num1),(CAMBIAR,num2)) -> do   -- Ambos cambian

      let nEnt1 = cambiar ent1 num1
      let nEnt2 = cambiar ent2 num2      
      
      
      mostrarCambio 1 nEnt1
      mostrarCambio 2 nEnt2
      verificaciones nEnt1 nEnt2

    ((ATACAR,num1) , (ATACAR,num2)) -> do   -- Ambos atacan

      let (nEnt1,nEnt2) = chequearVelocidad ent1 ent2

      if nEnt1 == ent1
        then do
          (ent1, ent2) <- atacar nEnt1 nEnt2 num1
          if not $ derrotado $ getActual ent2
            then do
              (ent2, ent1) <- atacar ent2 ent1 num2
              verificaciones ent1 ent2
            else do
              verificaciones ent1 ent2

        else do
          (ent1, ent2) <- atacar nEnt1 nEnt2 num2
          if not $ derrotado $ getActual ent2
            then do
              (ent2, ent1) <- atacar ent2 ent1 num2
              verificaciones ent1 ent2
            else do
              verificaciones ent1 ent2

    otherwise                       -> do 

      putStrLn "Introdujeron comandos invalidos"

  where
    -- Funcion que revisa si algun entrenador perdio. 
    -- En caso negativo ejecuta el siguiente turno
    verificaciones :: Entrenador -> Entrenador -> IO ()
    verificaciones ent1 ent2 = do
      if perdio ent1 
        then do
          putStr "Los pokemones de Entrenador 1 no pueden continuar."
          putStrLn "Entrenador 2 gana!"
        else if perdio ent2
          then do
            putStr "Los pokemones de Entrenador 2 no pueden continuar."
            putStrLn "Entrenador 1 gana!"
          else
            turno ent1 ent2

  
    -- Funcion que muestra el estado de cada pokemon por turno
    mostrarEstados :: Entrenador -> Entrenador -> IO ()
    mostrarEstados ent1 ent2 = do
     putStrLn $ "\nPokemon Entrenador 1"
     putStrLn $ show (getActual ent1)
     putStrLn $ "\nPokemon Entrenador 2"
     putStrLn $ show (getActual ent2)
    
-- Funcion que interactua con el usuario cuando se hace cambios
mostrarCambio :: Int -> Entrenador -> IO ()
mostrarCambio n ent = do
  putStrLn $ "\nEntrenador " 
    ++ show n 
    ++ " ha cambiado su pokemon por " 
    ++ nombreEsp (especie (getActual ent))
  
-- Funcion que imprime la informacion del rendido  
jugadaRendirse :: Int -> IO () 
jugadaRendirse num = 
  case num of 
    0         -> do putStrLn "Ambos jugadores se han rendido"
                    putStrLn "Juego terminado: Empate"
    1         -> do putStrLn "Entrenador 1 se ha rendido"
                    putStrLn "Entrenador 2 gana!"
    2         -> do putStrLn "Entrenador 2 se ha rendido"
                    putStrLn "Entrenador 1 gana!"
    otherwise ->    putStrLn ""
