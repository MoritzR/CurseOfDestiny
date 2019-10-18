{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module DataTypes where
import Control.Monad.State
import Control.Lens (makeLenses, Lens', _1, _2)

data CardEffect = OnPlay Action
    | OnTurnEnd Action
    | OnActivate Action
    | WhileOnField LastingEffect
    deriving Eq

data LastingEffect = IncreaseAttack Int
    deriving Eq

data Action = AddToField Card
    | DiscardFromHand Card
    | EndTurn
    | Choose [Action]
    | Destroy CardLens Card
    | DestroyOne CardLens
    | Draw PlayerLens
    | DirectAttack Card PlayerLens
    | Attack Card Card -- Attack Target Source

instance Show Action where
    show (AddToField c) = "AddToField " ++ show c
    show (DiscardFromHand c) = "DiscardFromHand " ++ show c
    show (EndTurn) = "EndTurn"
    show (Choose actions) = "Choose " ++ show actions
    show (Destroy _ c) = "Destroy " ++ show c
    show (DestroyOne _) = "DestroyOne"
    show (Draw _) = "Draw"
    show (Attack c1 c2) = "Attack " ++ show c1 ++ " " ++ show c2
    show (DirectAttack c _) = "DirectAttack from  " ++ show c
instance Eq Action where
    -- At the moment, there is no need to differentiate Actions
    _ == _ = True

type CardLens = Lens' GameState [Card]
type PlayerLens = Lens' GameState Player

type Players = (Player, Player)

data GameState = GameState {
    _players :: Players
} deriving (Show, Eq)

data Player = Player {
    _name :: String,
    _deck :: [Card],
    _hand :: [Card],
    _field :: [Card],
    _playerCreature :: PlayerCreature
} deriving (Show)
instance Eq Player where
    (==) = mapf _name (==)

data PlayerCreature = PlayerCreature {
    _playerCreatureId :: String,
    _hp :: Int
} deriving Show
instance Eq PlayerCreature where
    (==) = mapf _playerCreatureId (==)

data GameAction = Play Card
    | PlayFromHand Int
    | AnnounceAttack Int Int -- Attack Target Source
    | AnnounceDirectAttack Int -- Attack Source
    | ActivateFromField Int
    | Pass
    | EndRound
    deriving (Eq, Show)

data Card = Card {
    _cardId :: String,
    _cardName :: String,
    _features :: [Feature],
    _effects :: [CardEffect]
}
instance Show Card where
  show c = _cardName c ++  " (" ++ (show $ _features c) ++ ")"
instance Eq Card where
    (==) = mapf _cardId (==)

mapf :: (b -> a) -> (a -> a -> c) -> (b -> b -> c)
mapf f g = \e1 e2 -> g (f e1) (f e2)

data Feature = Spell
    | Creature Int
    deriving Eq
instance Show Feature where
    show Spell = "S,"
    show (Creature power) = "C[" ++ (show power) ++ "],"

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Card
makeLenses ''PlayerCreature

activePlayer :: Lens' GameState Player
activePlayer = players._1
enemyPlayer :: Lens' GameState Player
enemyPlayer = players._2
playerHp :: Lens' Player Int
playerHp = playerCreature.hp