module Library where
import PdePreludat

data Auto = Auto{
    color :: String,
    velocidad :: Number,
    distanciaRecorrida ::Number
} deriving (Show, Eq)

type Carrera = [Auto]

------- Punto 1 -------
distanciaEntre :: Auto -> Auto -> Number
distanciaEntre auto  = abs. (distanciaRecorrida auto -). distanciaRecorrida 

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (auto1 /= auto2) && distanciaEntre auto1 auto2 < 10

tieneAlgunoCerca :: Auto -> Carrera -> Bool
tieneAlgunoCerca auto = any(estaCerca auto)  

vaGanando::Auto -> Carrera -> Bool
vaGanando auto = all((porDelante auto).distanciaRecorrida).filter(/auto)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = not . tieneAlgunoCerca && vaGanando auto carrera

porDelante :: Auto -> Carrera -> Bool
porDelante auto = (<distanciaRecorrida auto).distanciaRecorrida

puesto :: Auto -> Carrera -> Number
puesto auto = (1+).length.filter(not. porDelante auto)

------- Punto 2 -------
correr :: Number -> Auto -> Auto
correr tiempo auto = auto {
    distanciaRecorrida = distanciaRecorrida auto + tiempo * velocidad auto
}

alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad modificador auto = auto {
    velocidad = (modificador. velocidad) auto
}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad nro = alterarVelocidad (max 0.substract nro)

{-bajarVelocidad auto nro = 
    |velocidad auto < nro = auto {velocidad = 0}
    |otherwhise = alterarVelocidad (+(-nro)) auto
-}

------- Punto 3 -------
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera -> Carrera
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50)

miguelitos :: Number -> Auto -> Carrera -> Carrera
miguelitos nro auto = afectarALosQueCumplen (porDelante auto) (bajarVelocidad nro)

aumento :: Number -> Auto -> Auto
aumento tiempo = (alterarVelocidad (/2)).(correr tiempo).(alterarVelocidad (*2))
--aumento tiempo = ((\ _ -> velocidad auto).(correr tiempo).(alterarVelocidad (*2))

jetPack :: Number -> Auto -> Carrera -> Carrera
jetPack tiempo auto =  afectarALosQueCumplen (== auto) (aumento tiempo auto)

------- Punto 4 -------
type Evento = Carrera -> Carrera 
type Color = String
type Tabla = [(Number, Color)]

aplicarListaEvento::  Carrera -> [Evento] -> Carrera
aplicarListaEvento (evento1:eventoM:eventoC) carrera = evento1  

simularCarrera :: Carrera -> [Evento] -> Tabla   --simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Number, Color)]
simularCarrera carrera eventos = map (aplicarListaEvento carrera) eventos
((puesto)(colorAuto))
       
------- Punto 5 -------

