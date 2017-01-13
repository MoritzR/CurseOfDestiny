module DataTypes where

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