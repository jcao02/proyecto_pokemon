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
  } deriving (Show) 

-- Declaracion de Ataques   
data Ataque = Ataque 
  { nombreAtaq :: String
  , tipo       :: Tipo  
  , fisico     :: Bool
  , pps        :: Int
  , pow        :: Int
  } deriving (Show, Eq)
  
-- Declaracion de Monstruo
data Monstruo = Monstruo
  { especie     :: Especie
  , sobreNombre :: String
  , nivel       :: Int
  , hpAct       :: Int
  , ataques     :: [Ataque] 
  , individual  :: Int
  , esfuerzo    :: Int
  } deriving(Show)
