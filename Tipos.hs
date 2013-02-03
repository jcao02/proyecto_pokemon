-- Declaraciones de Tipos de Datos 
module Tipos where
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
  
-- Declaracion de Especies  
data Especie = Especie
  { numero      :: Int 
  , nombreEsp   :: String
  , tipoElem    :: (Tipo, Tipo) 
  , hp          :: Int
  , ataque      :: Int
  , defensa     :: Int
  , ataqueEsp   :: Int
  , defensaEsp  :: Int
  , velocidad   :: Int 
  , prevolucion :: Monstruo   -- Entero que identifican al pokemon padre 
  , evolucion   :: Monstruo   -- Entero que identifica al pokemon hijo
  } deriving (Show) 

-- Declaracion de Ataques   
data Ataque = Ataque 
  { nombreAtaq :: String
  , tipo       :: Tipo
  , fisico     :: Bool
  , pps        :: Int
  , pow        :: Int
  } deriving (Show)
  
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
 
-- Funciones para acceder a ciertos atributos de un tipo

getHp :: Monstruo -> Int

getHp (Monstruo _ _ _ hp _ _ _) = hp
