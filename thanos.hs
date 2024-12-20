module Library where
import PdePreludat

data Personaje = UnPersonaje{
    nombre :: String,
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    planeta :: String
}deriving (Show, Eq)

data Guantelete = UnGuantelete{
    material :: String,
    gemas :: [String]
}deriving (Show, Eq)
 
type Universo = [Personaje] 
--------- Punto 1 ---------
guanteleteLleno :: Guantelete -> Bool
guanteleteLleno guante = (length . gemas) guante == 6

reduccion :: Universo -> Number -> Number -> Universo
reduccion universo x n = take (div n x) (personajes universo)
chasquear :: Universo -> Number -> Universo
chasquear universo x = ((reduccion universo x) . length . personajes) universo

chasquido :: Guantelete -> Universo -> Universo
chasquido guante universo = --dudo de la comparacion a la hora de hacer el chasquido
    |guante material == "uru" && guanteleteLleno guante =  chasquear universo 2 
    |otherwise = universo

--------- Punto 2 ---------
apto :: Universo -> Bool
apto universo = any (\p -> edad p < 45) universo

energiaTotal :: Universo -> Number
energiaTotal universo = sum . map energia . filter (\p -> length (habilidades p) > 1)

{-************************* SEGUNDA PARTE *************************-}

--------- Punto 3 ---------

type Gema = Personaje -> Personaje

quitarEnergia :: Number -> Gema
quitarEnergia x pj = UnPersonaje { energia = energia pj - x}

quitarHabilidad :: String -> Gema
quitarHabilidad habili pj = filter (/= habili) (habilidades pj)

mente :: Number -> Gema
mente x = quitarEnergia x

alma :: String -> Gema
alma habili =  quitarHabilidad habili . quitarEnergia 10

transportar :: String -> Gema
transportar nuevoPlaneta = UnPersonaje{ planeta = nuevoPlaneta } 

espacio :: String -> Gema
espacio planeta = transportar planeta . quitarEnergia 20

poder :: Gema
poder pj= 
    |(length . habilidades) pj <= 2 = UnPersonaje { energia = 0 ,habilidades = []}
    | otherwise = UnPersonaje { energia = 0}

quitarAniosVida :: Number -> Gema
quitarAniosVida x = UnPersonaje {edad = x}

tiempo :: Gema
tiempo pj = 
    |edad pj > 36 = (quitarEnergia 50 . quitarAniosVida . div (edad pj)) 2
    |otherwise = (quitarAniosVida 18 . quitarEnergia 50) pj

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema

--------- Punto 4 ---------

{- ???????????????????? 
GuanteDeGoma = UnGuantelete = {material = goma, gemas = [tiempo, alma, gemaLoca]}

goma :: Guantelete -> Gema
goma GuanteDeGoma = gemaLoca . alma "programacion en Haskell" . alma "usar Mjolnir" . tiempo
-}

--------- Punto 5 ---------
aplicarGema :: Personaje -> Gema -> Personaje
aplicarGema pj gema = gema pj

utilizar :: [Gema] -> Gema
utilizar gemas rival = foldl (aplicarGema) pj gemas

--------- Punto 6 ---------
gemaMasPoderosa :: [Gemas] -> Personaje -> Gema
gemaMasPoderosa (gema1 : gema 2 : gemas) pj = 
    |(energia . gema1) pj < (energia . gema2) pj = gemaMasPoderosa (gema1:gemas) pj
    |otherwise = gemaMasPoderosa (gema2:gemas) personajes
