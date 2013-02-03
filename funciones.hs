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
-- Falta ver bien la comparacion de tipos para el modificador!!!!!!!!!!!

damage :: Int -> Int -> Int -> Int -> Double -> Int

damage nvlA pow atq def modi = floor (modi * fromIntegral (div dividendo 50 + 2) )
  where 
  dividendo = ((div (2 * nvlA) 5) + 2) * pow * div atq def 
