module Batalla where

import Funciones
import Pokemon

-- Funciones...
-- Obtiene el pokemon actual de un entrenador
getActual :: Entrenador -> Monstruo
getActual ent = (!!) (pokemones ent) (actual ent)


-- Determina si el Monstruo esta conciente
estaConciente :: Monstruo -> Bool
estaConciente m = (hpAct m) > 0 


-- Determina si un entrenador puede atacar con su pokemon actual
puedeAtacar :: Entrenador -> Int -> Bool
puedeAtacar ent atq = pps (ataques (getActual ent) !! (atq - 1)) > 0 

-- Cambia el pokemon actual de un Entrenador
cambiarPokemon :: Entrenador -> Int -> Entrenador
cambiarPokemon ent n = ent { actual = (n - 1) }

-- Retorna en una tupla de monstruos el monstruo mas rapido de primero                                   
chequearVelocidad :: Entrenador -> Entrenador -> (Entrenador,Entrenador) 
chequearVelocidad ent1 ent2
  | v1 > v2   = (ent1,ent2)
  | v1 < v2   = (ent2,ent1)
  | otherwise = (ent1,ent2)
  
  where 
    v1   = velocidad (especie (getActual ent1))
    v2   = velocidad (especie (getActual ent2))
  
-- Funcion que determine si un entrenador perdio 
perdio :: Entrenador -> Bool
perdio ent = not $ all estaConciente (pokemones ent)

-- Actualiza la lista de pokemons con los vivos
chequearVivos :: Entrenador -> Int -> IO Entrenador 
chequearVivos ent num = do
  if not $ estaConciente $ getActual ent then do 
    nactual <- pedirPokemon ent num 
    return  ent { actual = nactual }
  else 
    return ent

-- Funcion que pide un pokemon a un entrenador hasta que sea valido
pedirPokemon :: Entrenador -> Int -> IO Int
pedirPokemon ent num = do
  let msj = "Entrenador " ++ (show num) ++ " su pokemon no puede continuar, elija uno nuevo" 
  putStrLn msj
  input <- getLine 
  let nuevo = read input :: Int
  if nuevo >= length (pokemones ent) then 
    pedirPokemon ent num
  else 
    if not $ estaConciente $ pokemones ent !! nuevo then pedirPokemon ent num
    else return nuevo
    
  
  
-- Ataque
atacar :: Entrenador -> Entrenador -> Int -> IO (Entrenador, Entrenador)
atacar ent1 ent2 atq = do
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
	  | fisico $  ataques (getActual ent1) !! (atq - 1) = estadistica 31 (ataque (especie (getActual ent1))) 255 (nivel (getActual ent1))
	  | otherwise                        = estadistica 31 (ataqueEsp (especie (getActual ent1))) 255 (nivel (getActual ent1))
            -- Si es fisico el ataque, se usa el atributo defensa, de lo conrario se usa la defensa especial
	dfse 
	  | fisico $  ataques (getActual ent1) !! (atq - 1) = estadistica 31 (defensa (especie (getActual ent2))) 255 (nivel (getActual ent2)) 
	  | otherwise                        = estadistica 31 (defensaEsp (especie (getActual ent2))) 255 (nivel (getActual ent2))
            
            -- Modificador de daño
	modi = modDano (especie (getActual ent1)) ((ataques (getActual ent1)) !! (atq-1)) (especie (getActual ent2)) 
