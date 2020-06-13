module Library where
import PdePreludat


-- Punto 1

type Nombre = String
type Direccion = String
type Fecha = (Number, Number, Number)
type CondicionViaje = Viaje -> Bool

data Chofer = Chofer {
    nombreChofer    :: Nombre,
    kmDelAuto       :: Number,
    viajes          :: [Viaje],
    condicionViaje  :: CondicionViaje
} deriving Show

data Viaje = Viaje {
    fecha   :: Fecha,
    cliente :: Cliente,
    costo   :: Number
} deriving Show

data Cliente = Cliente {
    nombreCliente   :: Nombre,
    direccion       :: Direccion
} deriving Show

-- Punto 2

tomaCualquierViaje :: CondicionViaje
tomaCualquierViaje _ = True

tomaViajePorPlata :: CondicionViaje
tomaViajePorPlata = (> 200).costo 

tomaViajePorNombre :: Number -> CondicionViaje
tomaViajePorNombre longitud = (longitud <).length.nombreCliente.cliente

tomaViajePorZona :: Direccion -> CondicionViaje
tomaViajePorZona zonaQueNo = (zonaQueNo /=).direccion.cliente

-- Punto 3

lucas = Cliente {
    nombreCliente   = "Lucas",
    direccion       = "Victoria"
}

daniel = Chofer {
    nombreChofer    = "Daniel",
    kmDelAuto       = 23.500,
    viajes          = [viajeDanielLucas],
    condicionViaje  = tomaViajePorZona "Olivos"
}

viajeDanielLucas = Viaje {
    costo   = 150,
    fecha   = (20,04,2017),
    cliente = lucas
}

alejandra = Chofer {
    nombreChofer    = "Alejandra",
    kmDelAuto       = 180000,
    viajes          = [],
    condicionViaje  = tomaCualquierViaje
}

-- Punto 4

puedeTomarViaje :: Chofer -> Viaje -> Bool
puedeTomarViaje = condicionViaje

-- Punto 5
liquidacionChofer :: Chofer -> Number
liquidacionChofer chofer = sum.map costo $ viajes chofer

-- Punto 6
-- 6.a
choferesQuePuedenTomarViaje :: Viaje -> [Chofer] -> [Chofer]
choferesQuePuedenTomarViaje viaje = filter (flip puedeTomarViaje viaje)

-- 6.b
-- considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
choferMenosViajero :: [Chofer] -> Chofer
choferMenosViajero [unChofer] = unChofer
choferMenosViajero (chofer1:chofer2:choferes) | cantidadViajes chofer1 < cantidadViajes chofer2 = choferMenosViajero (chofer1:choferes)
                                              | otherwise = choferMenosViajero (chofer2:choferes)
cantidadViajes :: Chofer -> Number
cantidadViajes = length . viajes

-- 6.c se considera que el viaje se aplica a un solo chofer ya que no tendria sentido
-- que muchos choferes hagan exactamente el mismo viaje

-- ¿Cómo logra representar este cambio de estado? 
-- Aunque haskell no cuenta con efecto colateral se genera un nuevo chofer con todos los mismos valores
-- que el chofer anterior excepto por el atributo de viajes que es una nueva lista con todos los viajes anteriores
-- junto al nuevo

type NuevoViaje = Viaje -> Chofer -> Chofer

agregarViaje :: NuevoViaje
agregarViaje viaje chofer = chofer {
    viajes = viaje:viajes chofer
}

efectuarViaje :: NuevoViaje
efectuarViaje viaje chofer | puedeTomarViaje chofer viaje = agregarViaje viaje chofer
                           | otherwise = chofer

-- Punto 7

viajeNitoLucas = Viaje {
    fecha = (11,03,2017),
    costo = 50,
    cliente = lucas
}

repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy = Chofer {
    nombreChofer    = "Nito Infy",
    kmDelAuto       = 70000,
    condicionViaje  = tomaViajePorNombre 3,
    viajes = repetirViaje viajeNitoLucas 
}
-- ¿Puede calcular la liquidación de Nito? Justifique.
-- No, la liquidacion de Nito no puede ser calculada ya que la funcion no diverge en ningun momento 

-- ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 
-- Si, se puede saber si Nito puede tomar un viaje de Lucas gracias a la evaluacion perezosa que se implementame
-- en Haskell, donde solo se busca cumplir con el objetivo solicitado y no se 'ignoran' el resto de campos.
-- Es decir, en este caso, solo se evaluaria la condicionViaje de Nito, y no se entraria en el loop infinito de viajes

-- Punto 8
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3
