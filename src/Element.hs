{-# LANGUAGE PatternSynonyms #-}
module Element where

data Element
  = Neutral
  | Feuer
  | Wald
  | Wasser
  | Wind
  | Licht
  | Tod
  | Doppel Element Element
  deriving (Eq, Show)

newtype Kosten = Kosten [ElementKosten]

pattern X :: Element -> Kosten
pattern X element = Kosten [VariableElementKosten element]
data ElementKosten = ElementKosten Int Element | VariableElementKosten Element | Nichts

infixr 8 //
(//) :: Element -> Element -> Element
(//) = Doppel

instance Num (Element -> Kosten) where
  fromInteger n e = Kosten [ElementKosten (fromInteger n) e]
  (+) = error "not used"
  (*) = error "not used"
  abs = error "not used"
  negate = error "not used"
  signum = error "not used"

instance Num Kosten where
  fromInteger n = Kosten [ElementKosten (fromInteger n) Neutral]
  (Kosten xs) + (Kosten ys) = Kosten $ xs ++ ys
  (*) = error "not used"
  abs = error "not used"
  negate = error "not used"
  signum = error "not used"

nichts :: Kosten
nichts = Kosten [Nichts]

test :: Kosten
test = 3 Wasser + 4 Feuer + 2 (Feuer // Wasser)
