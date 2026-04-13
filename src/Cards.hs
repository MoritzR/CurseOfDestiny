module Cards where

import Trigger
import CardEffect
import Element


data CardType
  = Allmagie
  | Gegenmagie
  | Magie
  | MagieDauerhaft
  | Wesen Stärke

type Stärke = Int

-- In my TGC, how do I best model different card types? In my game I have two main types of cards. Creatures and Spells. Creatures have power, e.g. 4000, and
--   can battle other creatures, but not spells. Some cards can target any kind of card, like "destroy one card" or "prevent a card from activating", while others
--   will only target either creatures or spells.

data Card = Card 
  { name :: String
  , cost :: Kosten
  , trigger :: Trigger
  }

-- series 26
edorsKonstruct = Card {
  name = "Edors Konstruct",
  cost = 1 Neutral,
  trigger = zahle (5 Neutral) do
    erhöhe Stärke EinAnderesWesen Dauerhaft 5000
}

