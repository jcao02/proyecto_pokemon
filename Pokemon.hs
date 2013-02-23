-- Declaraciones de Tipos de Datos 
module Pokemon where

-- Declaracion de Tipos de Pokemon
data Tipo
  = Bug
  | Dark
  | Dragon 
  | Electric 
  | Fighting
  | Fire 
  | Flying 
  | Ghost
  | Grass
  | Ground 
  | Ice 
  | Normal 
  | Poison 
  | Psychic
  | Rock 
  | Steel 
  | Water
  deriving (Bounded, Eq, Enum, Read, Show)
  
-- Relacion entre los tipos de ataques
relacionAtaqueTipo :: Tipo      -- Tipo de ataque a determinar la relación.
                   -> ( [Tipo]  -- Tipos debiles a el (2x dano). 
                      , [Tipo]  -- Tipos resistentes a el (0.5x dano).
                      , [Tipo]  -- Tipos inmunes a el (0x dano).
                      )
                      
relacionAtaqueTipo x
  | Bug      <- x = ([Grass, Psychic, Dark], [Fighting, Flying, Poison, Ghost, Steel, Fire], [])
  | Dark     <- x = ([Ghost, Psychic], [Fighting, Steel, Dark], [])
  | Dragon   <- x = ([Dragon], [Steel], [])
  | Electric <- x = ([Flying, Water], [Grass, Electric, Dragon], [Ground])
  | Fighting <- x = ([Normal, Rock, Steel, Ice, Dark], [Flying, Poison, Bug, Psychic], [Ghost])
  | Fire     <- x = ([Bug, Steel, Grass, Ice], [Rock, Fire, Water, Dragon], [])
  | Flying   <- x = ([Fighting, Bug, Grass], [Rock, Steel, Electric], [])
  | Ghost    <- x = ([Ghost, Psychic], [Steel, Dark], [Normal])
  | Grass    <- x = ([Ground, Rock, Water], [Flying, Poison, Bug, Steel, Fire, Grass, Dragon], [])
  | Ground   <- x = ([Poison, Rock, Steel, Fire, Electric], [Bug, Grass], [Flying])
  | Ice      <- x = ([Flying, Ground, Grass, Dragon], [Steel, Fire, Water], [])
  | Normal   <- x = ([], [Rock, Steel], [Ghost])
  | Poison   <- x = ([Grass], [Poison, Ground, Rock, Ghost], [Steel])
  | Psychic  <- x = ([Fighting, Poison], [Steel, Psychic], [Dark])
  | Rock     <- x = ([Flying, Bug, Fire, Ice], [Fighting, Ground, Steel], [])
  | Steel    <- x = ([Rock, Ice], [Steel, Fire, Water, Electric], [])
  | Water    <- x = ([Ground, Rock, Fire], [Water, Grass, Dragon], [])
  
  
-- Declaracion de Especies  
data Especie = Especie
  { numero      :: Int 
  , nombreEsp   :: String
  , tipoElem    :: [Tipo] 
  , hp          :: Int
  , ataque      :: Int
  , defensa     :: Int
  , ataqueEsp   :: Int
  , defensaEsp  :: Int
  , velocidad   :: Int 
  , prevolucion :: String   -- String que identifican al pokemon padre 
  , evolucion   :: String   -- String que identifica al pokemon hijo
  } deriving (Eq) 

-- Declaracion de Ataques   
data Ataque = Ataque 
  { nombreAtaq :: String
  , tipo       :: Tipo  
  , fisico     :: Bool
  , pps        :: Int
  , pow        :: Int
  } deriving (Eq)
  
-- Declaracion de Monstruo
data Monstruo = Monstruo
  { especie     :: Especie
  , sobreNombre :: String
  , nivel       :: Int
  , hpAct       :: Int
  , ataques     :: [Ataque] 
  , individual  :: Int
  , esfuerzo    :: Int
  } deriving(Eq)

-- Declaracion de Entrenador
data Entrenador = Entrenador
  { posicion    :: Int
  , actual      :: Int
  , pokemones   :: [Monstruo]
  } deriving Eq

-- Declaracion de instancias 
-- Instancia de show para Especie
instance Show Especie where 
  show (Especie num nomb tipo hp atk def eatk edef vel prev ev) = 
    "Numero de Especie: " ++ show num ++ "\n" 
    ++ "Nombre de Especie: " ++ show nomb ++ "\n"
    ++ "Tipos            : " ++ show tipo ++ "\n"
    ++ "Hp base          : " ++ show hp ++ "\n"
    ++ "Ataque base      : " ++ show atk ++ "\n"
    ++ "Defensa base     : " ++ show def ++ "\n"
    ++ "Ataque Especial  : " ++ show eatk ++ "\n"
    ++ "Defensa Especial : " ++ show edef ++ "\n" 
    ++ "Velocidad        : " ++ show vel ++ "\n" 
    ++ "Preevolucion     : " ++ show prev ++ "\n" 
    ++ "Evolucion        : " ++ show ev ++ "\n" 

-- Instancia de show para Monstruo  
instance Show Monstruo where
  show (Monstruo esp snom nvl hp atks ind esf) = 
    if hp < 0 
    then
      "Especie          : " ++ show (nombreEsp esp) ++ "\n"
      ++ "Nivel            : " ++ show nvl ++ "\n"
      ++ "Hp Actual        : " ++ show 0 ++ "\n"
    else
      "Especie          : " ++ show (nombreEsp esp) ++ "\n"
      ++ "Nivel            : " ++ show nvl ++ "\n"
      ++ "Hp Actual        : " ++ show hp ++ "\n"

-- Instancia de show para Ataque
instance Show Ataque where
  show (Ataque nomb tipo fis pps pow) =
    "Nombre   : " ++ show nomb ++ "\n"
    ++ "Tipo     : " ++ show tipo ++ "\n"
    ++ "Es fisico: " ++ show fis ++ "\n"
    ++ "PPs      : " ++ show pps ++ "\n"
    ++ "Poder    : " ++ show pow ++ "\n"

-- Instancia de show de Entrenador 
instance Show Entrenador where
  show (Entrenador pos act pok) =
    "Pokemon Actual:" ++ show act ++ "\n"
    ++ "Pokemones     :" ++ show pok ++ "\n"  

-- Funciones 

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

damage nvlA pow atq def modi = floor $ ((dividendo / 50) +2) * (fromIntegral modi)
  where
    dividendo = ((((fromIntegral (2 * nvlA)) / 5) + 2) * (fromIntegral pow) * (fromIntegral atq / fromIntegral def))


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
