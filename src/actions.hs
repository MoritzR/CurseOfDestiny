module Actions where

import DataTypes
import Control.Lens

resolve :: Action -> GameState -> GameState
resolve (AddToField c) g = over (activePlayer.field) (c:) g
resolve _ g = g