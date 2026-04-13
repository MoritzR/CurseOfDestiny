module Cards where

import Trigger
import CardEffect

data Card = Card 
  { name :: String
  , trigger :: Trigger
  }

edorsKonstruct = Card {
  name = "Edors Konstruct",
  trigger = zahle undefined $ ziehe 1
}
