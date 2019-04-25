-----------------------------------------------------------------------------------------------------------
--Tipos de datos propios con funciones como componente 
--Criterio de igualdad entre funciones

type Pirata = String 
type Tesoro = (String, Int)

----------------------------------
-- Sintaxis común
{- data Barco = UnBarco  String Pirata (Tesoro->Bool) 

nombre (UnBarco n _ _ ) = n
tripulacion (UnBarco  _ t _ ) = t
formaSaqueo (UnBarco _ _ f) = f
-}

----------------------------------
-- Sintaxis de registro

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
-- Enumeración

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes deriving (Show,Ord,Eq)

noMeGusta Lunes = True
noMeGusta x = False

nada _ = Lunes


-------------------------------------------------------------------------------------------------------------
-- Multiples constructores con sintaxis de registro
-- Criterio de ordenamiento 

data Jugador = 
    Campo {
        camiseta::Int, 
        goles::Int }| 
    Arco {
        camiseta::Int, 
        apodo:: String,
        recibidos:: Int} deriving Eq

messi = Campo 10 400
pipa = Campo 9 2
romero = Arco 1 "chiquito" 4

instance Show Jugador where
--    show jugador = show (camiseta jugador) -- Se muestran todos igual, con el nro de camiseta
    show (Arco _ apodo recibidos) = "Se trata del arquero conocido como " ++ apodo ++ ", al que le hicieron " ++ show recibidos ++ " goles"
    show (Campo _ goles) = "Jugador de campo que hizo " ++ show goles ++ " goles"

-- no hace falta pattern matching, camiseta se aplica sobre todos
esTitular jugador = (camiseta) jugador <= 11

-- pattern matching 
esBueno (Arco _ _ recibidos) = recibidos < 10
esBueno (Campo _ convertidos) = convertidos > 100

-- Criterio de orden para los jugadores, según indice de valoración
instance Ord Jugador where
    j1 <= j2 = valoracion j1 <= valoracion j2

valoracion (Arco camiseta apodo recibidos) = div (recibidos * 10 + length apodo)  (camiseta)
valoracion (Campo camiseta convertidos) = (convertidos * camiseta)

--Invocacion
--maximum  [Arco 2 "pato" 5, Campo 4 0,Campo 3 10]
--Jugador de campo que hizo 10 goles

-- Si hubiera una unica definicion para hacerParo, común a todos los trabajadores
--haceParo:: Trabajador a => a -> Bool
--haceParo _ = True

--laburar:: Trabajador a => a -> a
--laburar = id


-- Definición de una Type Class
-- Para contemplar diferentes formas de hacer paro y laburar
-- "Todo tipo que pretenda ser de la clase trabajador, tiene que poder decidir si hace paro o no y puede tener su propia forma de trabajar"

class Trabajador a where
    haceParo:: a -> Bool
    laburar:: a -> a

-- Implementación común a todos los tipos que sean instancia de Trabajador, a menos que lo definan explícitamente
    laburar = id

instance Trabajador Jugador where
    haceParo (Arco _ _ _ ) = True
    haceParo (Campo _ goles) = goles > 100
    laburar (Arco n apodo goles) = Arco n apodo (goles+1)
    laburar (Campo n goles) = Arco n "Me mandaron al arco" 0

-- Hacemos que los tipos básicos también sean "Trabajadores"
instance Trabajador Bool where
    haceParo unBool = not unBool

instance Trabajador Int where
    haceParo nro = nro < 5

-- También los días son trabajadores    
instance Trabajador Dia where
    haceParo Martes = True
    haceParo otroDia = False
    laburar Martes = Miercoles
    laburar otroDia = Martes

-- Consultas
-- haceParo messi
-- haceParo romero
-- haceParo Martes
-- haceParo True
-- haceParo 10
-- laburar messi
-- laburar romero
-- laburar Martes
-- (laburar.laburar) Martes
-- laburar True
-- laburar 10

