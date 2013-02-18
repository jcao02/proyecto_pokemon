module Batalla where

import Funciones
import Pokemon
-- Declaracion de tipo Entrenador
data Entrenador = Entrenador
  { actual    :: Int
  , pokemones :: [Monstruo]
  , rendido   :: Bool
  }

-- Funciones...

-- Determina si el Monstruo esta conciente
estaConciente :: Monstruo -> Bool

estaConciente m = (hpAct m) > 0 

-- Determina si el Ataque tiene suficientes PPs para atacar
puedeAtacar :: Ataque -> Bool

puedeAtacar a = (pps a) > 0 

-- Cambia el pokemon actual de un Entrenador
cambiarPokemon :: Entrenador -> Int -> Entrenador

cambiarPokemon ent p
  | p >= length (pokemones ent) = ent
  | otherwise                   = if not $ estaConciente $ (!!) (pokemones ent) p then
                                    ent
                                  else
                                    ent { actual = p }

-- Realiza un ataque de un Monstruo sobre otro

atacar :: Monstruo -> Monstruo -> Int -> Maybe (Monstruo, Monstruo)

atacar ofen def atq 
  | not $ puedeAtacar $ (!!) (ataques ofen) atq = Nothing  -- Si no se puede atacar por los PPs, retorna Nothing
  | otherwise                                   = Just (ofen { -- En otro caso, Se retorna la tupla del atacante y el defensor con PPs y hp actualizados 
                                                         ataques = nataques (ataques ofen) 
                                                         }, 
                                                         def { 
                                                         hpAct = nHp
                                                         }) 

  where
    -- Funcion que actualiza el ataque utilizado con el nuevo PPs de la lista de ataques el Monstruo Ofensivo
    nataques :: [Ataque] -> [Ataque]
    nataques []                    = []
    nataques (x:xs)
      | x == (!!) (ataques ofen) atq = x { pps = pps'-1 } : nataques xs
      | otherwise                    = x : nataques xs

      where
        pps' = pps $ (!!) (ataques ofen) atq

    -- Funcion que retorna la nueva hp dado el daño inflijido por el Pokemon Ofensivo al Defensivo 
    nHp = (hpAct def) - dano

      where 
        dano = damage lvl power atk dfse modi
          where
            lvl = nivel ofen
            power = pow $ (!!) (ataques ofen) atq
            -- Si es fisico el ataque, se usa el atributo ataque, de lo contrario se usa el Ataque especial
            atk
              | fisico $ (!!) (ataques ofen) atq = estadistica 31 (ataque (especie ofen)) 255 (nivel ofen)
              | otherwise                        = estadistica 31 (ataqueEsp (especie ofen)) 255 (nivel ofen)
            -- Si es fisico el ataque, se usa el atributo defensa, de lo conrario se usa la defensa especial
            dfse 
              | fisico $ (!!) (ataques ofen) atq = estadistica 31 (defensa (especie def)) 255 (nivel def) 
              | otherwise                        = estadistica 31 (defensaEsp (especie ofen)) 255 (nivel ofen)
            
            -- Modificador de daño
            modi = modDano (especie ofen) ((!!) (ataques ofen) atq) (especie def) 
