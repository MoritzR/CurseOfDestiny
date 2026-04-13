{-# LANGUAGE ConstraintKinds #-}

module DataTypesNew where

import Optics
import GHC.Generics (Generic)

data Aura

type Stärke = Int

data CardType

type CardLens = Lens' GameState [Card]

type PlayerLens = Lens' GameState Player

type Players = (Player, Player)

newtype GameState = GameState
  { players :: Players
  }

data Player = Player
  { name :: String,
    deck :: [Card],
    hand :: [Card],
    field :: [Card],
    playerCreature :: PlayerCreature
  }

data PlayerCreature = PlayerCreature
  { playerCreatureId :: String,
    hp :: Int
  }
  deriving (Show, Generic)

data GameAction
  = Play Card
  | PlayFromHand Int
  | AnnounceAttack Int Int -- Attack Target Source
  | AnnounceDirectAttack Int -- Attack Source
  | ActivateFromField Int
  | Pass
  | EndRound

data Card = Card
  { cardType :: CardType,
    cardId :: String,
    cardName :: String,
    effects :: String
  }
  deriving (Generic)


