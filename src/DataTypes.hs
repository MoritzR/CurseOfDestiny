module DataTypes where
import Control.Lens (Lens', _1, _2, (^.))
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Polysemy (Members, Sem)
import Polysemy.State (State)
import Polysemy.Input (Input)
import Polysemy.Trace (Trace)

type HasStateIO r = Members [State GameState, Input Int, Trace] r
type Game r a = HasStateIO r => Sem r a

data CardEffect = OnPlay Action
    | OnTurnEnd Action
    | OnActivate Action
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
    players :: Players
} deriving (Show, Eq, Generic)

data Player = Player {
    name :: String,
    deck :: [Card],
    hand :: [Card],
    field :: [Card],
    playerCreature :: PlayerCreature
} deriving (Show, Generic)
instance Eq Player where
    (==) = mapEq name

data PlayerCreature = PlayerCreature {
    playerCreatureId :: String,
    hp :: Int
} deriving (Show, Generic)
instance Eq PlayerCreature where
    (==) = mapEq playerCreatureId

data GameAction = Play Card
    | PlayFromHand Int
    | AnnounceAttack Int Int -- Attack Target Source
    | AnnounceDirectAttack Int -- Attack Source
    | ActivateFromField Int
    | Pass
    | EndRound
    deriving (Eq, Show)

data Card = Card {
    cardId :: String,
    cardName :: String,
    cardType :: CardType,
    effects :: [CardEffect]
} deriving Generic
instance Show Card where
  show c = cardName c ++  " (" ++ (show $ cardType c) ++ ")"
instance Eq Card where
    (==) = mapEq cardId

mapEq :: (Eq a, Eq b) => (b -> a) -> (b ->b -> Bool)
mapEq f = \e1 e2 -> (f e1) == (f e2)

data CardType = Spell
    | Creature Int
    deriving Eq
instance Show CardType where
    show Spell = "S,"
    show (Creature power) = "C[" ++ (show power) ++ "],"

activePlayer :: Lens' GameState Player
activePlayer = #players._1
enemyPlayer :: Lens' GameState Player
enemyPlayer = #players._2
playerHp :: Lens' Player Int
playerHp = #playerCreature . #hp
