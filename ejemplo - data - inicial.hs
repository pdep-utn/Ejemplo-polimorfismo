-- Inferencia de Tipos de datos
-- Tipos de datos variables
--

maximo::  Ord a => a -> a -> a
maximo a b 
  | a > b = a
  | otherwise = b

maximoRaro::  (Num a, Ord a) => a -> a -> a
maximoRaro a b
  | a > b = a + 1
  | otherwise = b - 1

--identidad :: Num a => a -> a
--identidad :: a -> a
identidad x = x 

chau 0 = length hola
chau x = x

hola = "hola como estas"

--operar :: Num a => a -> a -> a
--operar :: Int -> Int-> Int
-- operar a b =  div a b
-- operar a b =  a / b
operar a b =  a + b

-- ++
-- length

-- Definicion de tipos de datos propios 

--type String = [Char]
--type Persona = (String, String, Int)

--data Persona = UnaPersona String String Int  
--    deriving (Eq)

data Persona = UnaPersona { 
  nombre::String,
  apellido:: String ,
  anioNacimiento:: Int } deriving Eq

juan = UnaPersona "Juan" "Perez" 2000
maria = UnaPersona { nombre = "Maria", apellido = "Perez", anioNacimiento = 1990}

edad:: Persona -> Int
edad persona = 2019 - anioNacimiento persona

-- Simular efecto
rejuvenecer:: Int -> Persona -> Persona
rejuvenecer anios (UnaPersona nombre apellido anioNacimiento)= UnaPersona nombre (apellido++ " jr") (anioNacimiento + anios)

rejuvenecer':: Int -> Persona -> Persona
rejuvenecer' anios alguien = (quitarAnios anios.agregarApellido "jr") alguien

quitarAnios:: Int -> Persona -> Persona
quitarAnios anios alguien = alguien{anioNacimiento = anioNacimiento alguien + anios}

agregarApellido:: String -> Persona -> Persona
agregarApellido sufijo alguien = alguien{apellido = apellido alguien ++ sufijo}


-- Consultas
-- maria
-- Soy Maria Perez, tengo 29
-- rejuvenecer 10 maria
-- Soy Maria Perez jr, tengo 19

--anioNacimiento:: Persona -> Int
--anioNacimiento (UnaPersona _ _ an) = an

--nombre:: Persona -> String
--nombre  (UnaPersona n _ _ ) = n

instance Ord Persona where
     fulano <= mengano = edad fulano <= edad mengano

instance Show Persona where
     show persona = "Soy " ++ nombre persona ++ " " ++ apellido persona ++ ", tengo " ++ show (edad persona)
    

{-


instance Eq Persona where
    fulano == mengano = nombre fulano == nombre mengano
    




-}