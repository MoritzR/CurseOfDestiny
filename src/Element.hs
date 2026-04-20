module Element where
import DataTypesNew (Element(..), Kosten(..), ElementKosten(..))

infixr 8 //
(//) :: Element -> Element -> Element
(//) = Doppel

nichts :: Kosten
nichts = Kosten [Nichts]

test :: Kosten
test = 3 Wasser + 4 Feuer + 2 (Feuer // Wasser)
