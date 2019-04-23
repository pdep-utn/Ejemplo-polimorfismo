-----------------------------------------------------------------------------------------------------------
--Tipos de datos propios con funciones como componente 
--Criterio de igualdad entre funciones

type Pirata = String --Simplificado del TP
type Tesoro = (String, Int)

data Barco = UnBarco {
    nombre:: String,
    tripulacion:: [Pirata],
    formaSaqueo :: Tesoro->Bool
} 

-- Dos barcos son iguales cuando se llaman igual, la tripulación es la misma y la forma de saqueo es la misma
-- ¿como saber si dos funciones son la misma?
-- No se puede. Una aproximación es aplicarla y ver si devuelve lo mismo 

instance Eq Barco where 
    b1 == b2 = nombre b1 == nombre b2 && (tripulacion b1) == (tripulacion b2) && mismaFormaSaqueo b1 b2

tesorosDePrueba = [("oro",1000),("espada",10)]    
mismaFormaSaqueo b1 b2 = filter (formaSaqueo b1) tesorosDePrueba == filter (formaSaqueo b2) tesorosDePrueba

-- Invocacion
--UnBarco "perla negra" [] (const False)  == UnBarco "perla negra" [] (const True)
--False

-------------------------------------------------------------------------------------------------------------
-- Multiples constructores
-- Criterio de ordenamiento 

data Jugador = 
    Campo {
        camiseta::Int, 
        goles::Int }| 
    Arco {
        camiseta::Int,
        apodo:: String,
        recibidos:: Int} deriving Eq

instance Show Jugador where
--    show jugador = show (camiseta jugador) -- Se muestran todos igual, con el nro de camiseta
    show (Arco _ apodo recibidos) = "Se trata del arquero conocido como " ++ apodo ++ ", al que le hicieron " ++ show recibidos ++ " goles"
    show (Campo _ goles) = "Jugador de campo que hizo " ++ show goles ++ " goles"

-- no hace falta pattern matching, camiseta se aplica sobre todos
esTitular jugador = camiseta jugador <= 11

-- pattern matching 
esBueno (Arco _ _ recibidos) = recibidos < 10
esBueno (Campo _ convertidos) = convertidos > 100

-- Criterio de orden para los jugadores, según indice de valoración
instance Ord Jugador where
    j1 <= j2 = valoracion j1 <= valoracion j2

valoracion (Arco camiseta apodo recibidos) = div (recibidos * 10 + length apodo)  camiseta
valoracion (Campo camiseta convertidos) = (convertidos * camiseta)

--Invocacion
--maximum  [Arco 2 "pato" 5, Campo 4 0,Campo 3 10]
--Jugador de campo que hizo 10 goles