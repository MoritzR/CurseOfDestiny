module DataTypes where
import Control.Monad.State

data GameState = GameState {
    players :: [Player],
    activePlayer :: Int
} deriving (Show)

data Player = Player {
    playerID :: Int,
    playerName :: String,
    deck :: [Card],
    hand :: [Card]
} deriving (Show)

data Card = Card {
    cardName :: String
} deriving (Show)

type GameStateIO a = StateT GameState IO a