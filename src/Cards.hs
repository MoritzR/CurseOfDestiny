module Cards where

import Trigger
import CardEffect
import Element

data Card = Card 
  { name :: String
  , trigger :: Trigger
  }

edorsKonstruct = Card {
  name = "Edors Konstruct",
  -- trigger = zahle (5 Neutral) $ erhöheStärke EinAnderesWesen Dauerhaft 5000
  trigger = zahle (5 Neutral) do
    erhöhe Stärke EinAnderesWesen Dauerhaft 5000
}
