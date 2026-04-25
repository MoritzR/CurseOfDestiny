module Element where

import DataTypes (Anzahl (..), Element (..), ElementKosten (..), Kosten (..), anzahlToInt)

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
    VariableElementKosten _element -> (+ PlaceHolder "X")
    ElementKosten anzahl _element -> (+ anzahl)
