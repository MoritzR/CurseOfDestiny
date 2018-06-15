module Actions where

import DataTypes
import Control.Lens

data Action = AddToField Card

resolve :: Action -> GameState -> GameState
resolve (AddToField c) g = over (activePlayer.field) (c:) g
resolve _ g = g