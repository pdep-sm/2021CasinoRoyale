module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]

type Carta = (Number, String)

type Mano = [Carta]
type Nombre = String
type Bebida = String

data Jugador = Jugador {
    nombre :: Nombre,
    mano :: Mano,
    bebida :: Bebida
} deriving Show

ocurrenciasDe x = length . filter (== x)

concatenar = foldl (++) []

-- 1.a
mayorSegun f x y
    | f x > f y = x
    | otherwise = y

-- 1.b
maximoSegun _ [x]      = x
maximoSegun f (x:y:xs) = maximoSegun f (mayorSegun f x y : xs)

-- 1.c
sinRepetidos []     = []
sinRepetidos (x:xs) = x : (sinRepetidos.filter (/= x)) xs

-- 2.a
esoNoSeVale carta = numeroFueraDeRango carta || paloInvalido carta
numeroFueraDeRango carta = not . elem (fst carta) $ [1..13]
paloInvalido carta = not . elem (snd carta) $ palos

-- 2.b
manoNegra jugador = length cartas /= 5 || any esoNoSeVale cartas
    where cartas = mano jugador

-- 3
seRepiteVecesEn cantidad lista elemento = ocurrenciasDe elemento lista == cantidad

juego :: Number -> Mano -> Bool
juego cantidad cartas = any (seRepiteVecesEn cantidad numeros) numeros
    where numeros = map fst cartas

par     = juego 2
pierna  = juego 3
poker   = juego 4
fullHouse cartas = par cartas && pierna cartas
color :: Mano -> Bool
color cartas = all (== head colores) colores
    where colores = map snd cartas
otro :: Mano -> Bool
otro _ = True