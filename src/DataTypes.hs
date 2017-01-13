{-# LANGUAGE TemplateHaskell #-}

module DataTypes where
import Control.Monad.State
import Control.Lens

type Players = (Player, Player)
type GameStateIO a = StateT GameState IO a

data GameState = GameState {
    _players :: Players,
    _activePlayer :: Int
} deriving (Show)

data Player = Player {
    _playerName :: String,
    _deck :: [Card],
    _hand :: [Card]
} deriving (Show)

data Card = Card {
    _cardName :: String
}
instance Show Card where
  show c = _cardName c

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Card
