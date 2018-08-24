{-# LANGUAGE TemplateHaskell #-}

module DataTypes where
import Control.Monad.State
import Control.Lens (makeLenses, Lens', _1, _2)

data CardEffect = OnPlay Action
    | OnTurnEnd Action
    | OnActivate Action
    deriving Eq

data Action = AddToField Card
    | DiscardFromHand Card
    | EndTurn
    | Choose [Action]
    | Destroy Card
    | Attack Card Card -- Attack Target Source
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
    | AnnounceAttack Int Int -- Attack Target Source
    | ActivateFromField Int
    | Pass
    | EndRound
    deriving Eq

data Card = Card {
    _cardId :: String,
    _cardName :: String,
    _features :: [Feature],
    _effects :: [CardEffect]
}
instance Show Card where
  show c = _cardName c ++  " (" ++ (show $ _features c) ++ ")"
instance Eq Card where
    (==) = mapEq _cardId (==)

mapEq :: (Eq a, Eq b) => (b -> a) -> (a -> a -> Bool) -> (b ->b -> Bool)
mapEq f eq = \e1 e2 -> (f e1) == (f e2)

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
enemyPlayer :: Lens' GameState Player
enemyPlayer = players._2