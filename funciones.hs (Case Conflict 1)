-- Modulo de funciones para calcular atributos de los pokemones
module Funciones where
import Pokemon
-- Archivo que contiene funciones para calcular ciertos atributos


-- Funcion que calcula el maximo de hp de un pokemon
maxHp :: Int -> Int -> Int -> Int -> Int

maxHp ivHp baseHp evHp nvl = div (dividendo * nvl) 100 + 10 
  where 
  dividendo = ivHp + (2 * baseHp) + (div evHp 4) + 100


--Funcion que calcula las estadisticas de un pokemon
estadistica :: Int -> Int -> Int -> Int -> Int

estadistica iv base ev nvl = div (dividendo * nvl) 100 +5
  where 
  dividendo = iv + (2 * base) + div ev 4


-- Funcion que calcula el daño de un ataque
damage :: Int -> Int -> Int -> Int -> Int -> Int

damage nvlA pow atq def modi = modi * fromIntegral (div dividendo 50 + 2)
  where 
  dividendo = ((div (2 * nvlA) 5) + 2) * pow * div atq def 

--Retorna el modificador de daño, recibiendo las especies de los Monstruos que pelean y el ataque realizado
modDano :: Especie -> Ataque -> Especie -> Int

modDano ofensivo atq defensivo 
  | mismoTipo (tipoElem ofensivo) = floor $ 1.5 * compararTipo (tipoElem defensivo)
  | otherwise                     = floor $ compararTipo (tipoElem defensivo)

  where
    -- Se compara el tipo del ataque con el tipo del atacante
    mismoTipo xs = foldl (||) False (map (\x -> x == tipo atq) xs)  

    -- Se compara el tipo del ataque con la lista de tipos del defensor
    compararTipo []     = 1.0
    compararTipo (x:xs) = cantidadDano (relacionAtaqueTipo $ tipo atq) x * compararTipo xs

      where
        -- Retorna la cantidad de daño dado por la tupla y el tipo
        cantidadDano (x, y, z) tipo 
          | elem tipo x = 2.0
          | elem tipo y = 0.5
          | elem tipo z = 0.0
          | otherwise   = 1.0
