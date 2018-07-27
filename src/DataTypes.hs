{-# LANGUAGE TemplateHaskell #-}

module DataTypes where
import Control.Monad.State
import Control.Lens

data CardEffect = OnPlay Action
    | OnTurnEnd Action
    deriving Eq

data Action = AddToField Card
    | EndTurn
    | Choose [Action]
    deriving (Show, Eq)

type Players = (Player, Player)

data GameState = GameState {
    _players :: Players
} deriving (Show, Eq)

data Player = Player {
    _name :: String,
    _deck :: [Card],
    _hand :: [Card],
    _field :: [Card]
} deriving (Show, Eq)

data GameAction = Play Card
    | PlayFromHand Int
    | Pass
    | EndRound
    deriving Eq

data Card = Card {
    _cardName :: String,
    _features :: [Feature],
    _effects :: [CardEffect]
}   deriving Eq
instance Show Card where
  show c = _cardName c ++  " (" ++ (show $ _features c) ++ ")"

data Feature = Spell
    | Creature Int
    deriving Eq
instance Show Feature where
    show Spell = "S,"
    show (Creature power) = "C[" ++ (show power) ++ "],"

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Card

activePlayer :: Lens' GameState Player
activePlayer = players._1
