module Spec where
import PdePreludat
import Library
import Test.Hspec

pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]
cartaInvalida = last piernaDeNueves
cartaValida = head pokerDeAses

jamesBond = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre = Jugador {
    nombre = "Le Chiffre",
    bebida = "Gin",
    mano = fullDeJokers
}
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

mesa1 = [jamesBond, leChiffre, felixLeiter]
mesaSolitario = [jamesBond]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1" $ do
    it "El mayor según valor absoluto entre -10 y 4 es -10" $ do
      mayorSegun abs (-10) 4 `shouldBe` (-10)
    it "El mayor según valor absoluto entre 4 y -10 es -10" $ do
      mayorSegun abs  4 (-10) `shouldBe` (-10)
    it "El máximo de la lista [1, 2, (-3)] según valor absoluto es -3" $ do
      maximoSegun abs [1, 2, (-3)] `shouldBe` (-3)
    it "La lista [1,1,3,3,4,4,1,2,2,1,1,2] sin repetidos es [1,3,4,2]" $ do
      sinRepetidos [1,1,3,3,4,4,1,2,2,1,1,2] `shouldBe` [1,3,4,2]
  describe "Punto 2" $ do
    it "esoNoSeVale se cumple para el 4 de copas" $ do
      cartaInvalida `shouldSatisfy` esoNoSeVale
    it "esoNoSeVale no se cumple para el 1 de corazones" $ do
      cartaValida `shouldNotSatisfy` esoNoSeVale
    it "Felix Leiter tiene una mano negra" $ do
      felixLeiter `shouldSatisfy` manoNegra
    it "James Bond no tiene una mano negra" $ do
      jamesBond `shouldNotSatisfy` manoNegra
  describe "Punto 3" $ do
    it "El juego con 4 ases es un poker" $ do
      pokerDeAses `shouldSatisfy` poker
  describe "Punto 4" $ do
    it "Alguien se carteo en la mesa1 usando primera def" $ do
      mesa1 `shouldSatisfy` alguienSeCarteo
    it "Alguien se carteo en la mesa1 usando segunda def" $ do
      mesa1 `shouldSatisfy` alguienSeCarteo'
    it "Nadie se carteo en la mesa de James Bond solito" $ do
      mesaSolitario `shouldNotSatisfy` alguienSeCarteo