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
maximoSegun' _ [x]      = x
maximoSegun' f (x:y:xs) = maximoSegun' f (mayorSegun f x y : xs)

maximoSegun f = foldl1 (mayorSegun f)
-- maximoSegun'' f (x:xs) = foldl (mayorSegun f) x xs

-- 1.c
sinRepetidos []     = []
sinRepetidos (x:xs) = x : (sinRepetidos.filter (/= x)) xs

-- 2.a
esoNoSeVale carta = numeroFueraDeRango carta || paloInvalido carta
numeroFueraDeRango carta = not . elem (fst carta) $ [1..13]
numeroFueraDeRango' = not . flip elem [1..13] . fst
numeroFueraDeRango'' = not . (`elem` [1..13]) . fst
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

-- 4 Si una carta se repite
alguienSeCarteo mesa = cartasEnJuego /= sinRepetidos cartasEnJuego
    where cartasEnJuego = concatenar . map mano $ mesa

alguienSeCarteo' mesa = any ((>1).flip ocurrenciasDe cartasEnJuego) cartasEnJuego
    where cartasEnJuego = concatenar . map mano $ mesa

-- flip funcion primero segundo = funcion segundo primero
-- ocurrenciasDe elemento lista

-- 5.a
{-
Dada la siguiente lista de valores para los distintos juegos:
valores = [(par,1), (pierna,2), (color,3), (fullHouse,4), (poker,5), (otro, 0)]
Definir valor/1 que, dada una lista de cartas, nos indique el valor del mismo,
que es el máximo valor entre los juegos que la lista de cartas cumple.
-}
valores = [(par, 1), (pierna, 2), (color, 3), (fullHouse, 4), (poker, 5), (otro, 0)]

valor mano = snd . maximoSegun snd . filter (($mano).fst) $ valores
valor' mano = maximum . map snd . filter (($mano).fst) $ valores
-- funcion $ parametro
-- Hacer más adelante definicion con fold :) 


-- 5.b
{- bebidaWinner/1, que dada una lista de jugadores nos devuelve la bebida de aquel 
jugador que tiene el juego de mayor valor, pero sin considerar a aquellos que tienen 
manos mal armadas. -}
bebidaWinner = bebida . maximoSegun (valor.mano) . filter (not.manoNegra)

-- 6
{-
Realizar las consultas que indiquen:
- El nombre del jugador que está tomando la bebida de nombre más largo.
- El jugador con mayor cantidad de cartas inválidas.
- El jugador de nombre más corto.
- El nombre del ganador de una mesa, que es aquel del jugador con el juego de mayor valor.
Nota: No definir funciones auxiliares para este punto. Construir las consultas 
únicamente en base a las funciones definidas para puntos anteriores.
-}
--  nombre . maximoSegun (length.bebida) $ mesa
--  maximoSegun (length. filter esoNoSeVale . mano) $ mesa
--  maximoSegun (negated . length . nombre) $ mesa
--  nombre . maximoSegun (valor.mano) $ mesa
