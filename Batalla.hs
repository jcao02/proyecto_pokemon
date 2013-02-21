module Batalla where

import Funciones
import Pokemon
-- Declaracion de tipo Entrenador
data Entrenador = Entrenador
  { posicion    :: Int
  , actual      :: Int
  , pokemones   :: [Monstruo]
  , rendido     :: Bool
  } deriving Show

  
-- Funciones...
-- Obtiene el pokemon actual de un entrenador
getActual :: Entrenador -> Monstruo
getActual ent = (!!) (pokemones ent) (actual ent)


-- Determina si el Monstruo esta conciente
estaConciente :: Monstruo -> Bool
estaConciente m = (hpAct m) > 0 


-- Cambia el pokemon actual de un Entrenador
cambiarPokemon :: Entrenador -> Int -> Entrenador
cambiarPokemon ent p
  | p >= length (pokemones ent) = ent
  | otherwise                   = if not $ estaConciente $ (!!) (pokemones ent) (p - 1) then
                                    ent
                                  else
                                    ent { actual = (p - 1) }

                                    
chequearVelocidad :: Entrenador -> Entrenador -> (Monstruo,Monstruo) 
chequearVelocidad ent1 ent2
  | v1 > v2   = (mon1,mon2)
  | v1 < v2   = (mon2,mon1)
  | otherwise = (mon1,mon2)
  
  where 
    v1   = velocidad (especie (getActual ent1))
    v2   = velocidad (especie (getActual ent2))
    mon1 = getActual ent1
    mon2 = getActual ent2
  

-- Actualiza la lista de pokemons con los vivos
chequearVivos :: Entrenador -> Entrenador 
chequearVivos ent = ent { pokemones = nPok }
  where
    nPok = foldl (\r x -> if estaConciente x then (x:r) else r) [] (pokemones ent)
  
  
-- Ataque
atacar :: Entrenador -> Entrenador -> Int -> IO (Entrenador, Entrenador)
atacar ent1 ent2 atq = do
  let 
  return $ (ent1 { pokemones = npokemones (pokemones ent1) },  ent2 { pokemones = defpokemones (pokemones ent2)})
  where
    -- Funcion que actualiza lista de pokemones del entrenador que ataco
    npokemones = foldl  (\r x -> if x == (!!) (pokemones ent1) (actual ent1) then x { ataques = nataques $ ataques x } : r else x:r)  []
    -- Funcion que actualiza lista de pokemones del entrenador defensor
    defpokemones = foldl  (\r x -> if x == (!!) (pokemones ent1) (actual ent1) then x { hpAct = nHp } : r else x:r)  []
    -- Funcion que actualiza el ataque utilizado con el nuevo PPs de la lista de ataques el Monstruo Ofensivo
    nataques = foldl  (\r x -> if x == (!!) (ataques (getActual ent1)) atq then x { pps = (pps x) - 1 } : r else x:r)  []
    -- Funcion que retorna la nueva hp dado el daño inflijido por el Pokemon Ofensivo al Defensivo 
    nHp = (hpAct (getActual ent2)) - dano
      where 
        dano = damage lvl power atk dfse modi
          where
            lvl = nivel (getActual ent1)
            power = pow $ (!!) (ataques (getActual ent1)) atq
            -- Si es fisico el ataque, se usa el atributo ataque, de lo contrario se usa el Ataque especial
            atk
              | fisico $ (!!) (ataques (getActual ent1)) atq = estadistica 31 (ataque (especie (getActual ent1))) 255 (nivel (getActual ent1))
              | otherwise                        = estadistica 31 (ataqueEsp (especie (getActual ent1))) 255 (nivel (getActual ent1))
            -- Si es fisico el ataque, se usa el atributo defensa, de lo conrario se usa la defensa especial
            dfse 
              | fisico $ (!!) (ataques (getActual ent1)) atq = estadistica 31 (defensa (especie (getActual ent2))) 255 (nivel (getActual ent2)) 
              | otherwise                        = estadistica 31 (defensaEsp (especie (getActual ent2))) 255 (nivel (getActual ent2))
            
            -- Modificador de daño
            modi = modDano (especie (getActual ent1)) ((!!) (ataques (getActual ent1)) atq) (especie (getActual ent2)) 
