-- Inferencia de Tipos de datos
-- Tipos de datos variables

maximo a b 
  | a > b  = a
  | otherwise = b

--identidad :: String->String
identidad x = x 

hola = "hola como estas"

--operar :: Num a => a -> a -> a
--operar :: Int -> Int-> Int
-- operar a b =  div a b
-- operar a b =  a / b
operar a b =  a + b

-- ++
-- length

-- Definicion de tipos de datos propios 
-- Simular efecto

--type String = [Char]
--type Persona = (String, String, Int)

--data Persona = UnaPersona String String Int  
--    deriving Eq

data Persona = UnaPersona { 
    nombre::String,
    apellido:: String,
    anioNacimiento:: Int } 

instance Show Persona where
    show persona = "Soy " ++ nombre persona ++ " " ++ apellido persona

instance Ord Persona where
    (<=) fulano mengano = edad fulano <= edad mengano

instance Eq Persona where
    fulano == mengano = nombre fulano == nombre mengano
    
edad:: Persona -> Int
edad persona = 2018 - anioNacimiento persona

--anioNacimiento:: Persona -> Int
--anioNacimiento (UnaPersona _ _ _ an) = an

--nombre:: Persona -> String
--nombre  (UnaPersona n _ _ _) = n

rejuvenecer:: Int -> Persona -> Persona
rejuvenecer anios (UnaPersona nombre apellido anioNacimiento)= UnaPersona nombre (apellido++ " jr") (anioNacimiento + anios)
