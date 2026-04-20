module Element where

import DataTypesNew (Anzahl (..), Element (..), ElementKosten (..), Kosten (..))

infixr 8 //
(//) :: Element -> Element -> Element
(//) = Doppel

nichts :: Kosten
nichts = Kosten [Nichts]

gesamtKosten :: Kosten -> Int
gesamtKosten (Kosten alleElemente) = anzahlToInt $ foldr sumCosts 0 alleElemente
 where
  sumCosts elementKosten = case elementKosten of
    Nichts -> (+ 0)
    VariableElementKosten _element -> (+ PlaceHolderX)
    ElementKosten anzahl _element -> (+ anzahl)
  anzahlToInt = \case
    Actual i -> i
    PlaceHolderX -> 0
    Mul a b -> anzahlToInt a * anzahlToInt b
    Add a b -> anzahlToInt a + anzahlToInt b
    Neg a -> negate $ anzahlToInt a
