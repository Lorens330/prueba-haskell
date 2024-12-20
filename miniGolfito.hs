module Library where
import PdePreludat

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

type Puntos = Number

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--------- Punto 1 ---------
type Palo = Habilidad -> Tiro
putter :: Palo
putter habilidad = UnTiro{
    velocidad = 10,
    precision = (precisionJugador habilidad) * 2 ,
    altura = 0
}
madera :: Palo
madera habilidad = UnTiro{
    velocidad = 100,
    precision = (precisionJugador habilidad) `div`  2 ,
    altura = 5   
}
hierros :: Number -> Palo
hierros n habilidad = UnTiro{
    velocidad = (fuerzaJugador habilidad) * n,
    precision = (precisionJugador habilidad) * n ,
    altura = (n-3) `max` 0
}

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

--------- Punto 2 ---------

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo . habilidad) jugador

--------- Punto 3 ---------
type Obstaculo = Tiro -> Tiro
tiroDetenido = UnTiro 0 0 0
{-
superaObstaculo :: (Tiro -> Bool) -> Tiro -> Tiro -> Obstaculo
superaObstaculo condicion efecto tiro =
    |condicion tiro = efecto tiro
    |otherwise = tiroDetenido 

esMayor :: Number -> Number -> Bool
esMayor x y = x < y

razSuelo:: Number -> Bool
razSuelo x = x==0 

superaTunel :: Tiro -> Bool
superaTunel tiro = (esMayor 90 . precision) tiro && (razSuelo . altura) tiro
efectoTunel :: Tiro -> Tiro 
efectoTunel tiro = UnTiro{ velocidad = (velocidad tiro) * 2, precision = 100, altura = 0}

tunel :: Obstaculo
tunel  = superaObstaculo  superaTunel efectoTunel

superaLaguna :: Tiro -> Bool
superaLaguna tiro = (esMayor 80 . velocidad) tiro && (between 1 5 . altura) tiro
efectoLaguna :: Number -> Tiro -> Tiro 
efectoLaguna tiro = UnTiro{velocidad = velocidad tiro, precision = precision tiro, altura =  (altura tiro) `div` largo}

laguna :: Number -> Obstaculo
laguna = superaObstaculo superaLaguna (efectoLaguna largo) 

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && (razSuelo . altura) tiro && (esMayor 95 .precision) tiro
hoyo :: Obstaculo
hoyo _ = tiroDetenido

--------- Punto 4 ---------

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles  jugador obstaculo = filter  (supera jugador obstaculo)  palos

supera :: Jugador -> Obstaculo -> Palo -> Bool
supera  jugador obstaculo = obstaculo . golpe jugador
--b
takeWhile :: (a -> Bool) -> [a] -> [a]

obstaculosSuperados ::[Obstaculo] -> Tiro -> Number
obstaculosSuperados obstaculos tiro = length . filter puedeSuperar obstaculos

--------- Punto 5 ---------





type Obstaculo = Tiro -> Tiro

tunel :: Obstaculo
tunel tiro =
    |(precision tiro) > 90 && (altura tiro) == 0 = UnTiro{ velocidad = (velocidad tiro) * 2, precision = 100, altura = 0}
    |otherwise = tiroDetenido

laguna :: Number -> Obstaculo
laguna largo tiro = 
    |(velocidad tiro) > 80 && between 1 5 (altura tiro) = 
        UnTiro{velocidad = velocidad tiro, precision = precision tiro, altura =  (altura tiro) `div` largo}
    |otherwise = tiroDetenido

hoyo :: Obstaculo
hoyo tiro = tiroDetenido -- between 5 20 (velocidad tiro) && (altura tiro) ==0 && (precision tiro) > 95  
-}