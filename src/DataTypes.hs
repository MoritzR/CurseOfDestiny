{-# LANGUAGE TemplateHaskell #-}

module DataTypes where
import Control.Monad.State
import Control.Lens

data CardEffect = OnPlay Action
    | OnTurnEnd Action

data Action = AddToField Card
    | EndTurn
    | Choose [Action]
    deriving Show

type Players = (Player, Player)

data GameState = GameState {
    _players :: Players
} deriving (Show)

data Player = Player {
    _name :: String,
    _deck :: [Card],
    _hand :: [Card],
    _field :: [Card]
} deriving (Show)

data GameAction = Play Card
    | PlayFromHand Int
    | Pass
    | EndRound

data Card = Card {
    _cardName :: String,
    _effects :: [CardEffect]
}
instance Show Card where
  show c = _cardName c

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Card

activePlayer :: Lens' GameState Player
activePlayer = players._1
