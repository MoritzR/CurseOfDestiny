module GameState where

import Cards (series26)
import DataTypes
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, gets)
import Optics (Lens', lensVL)

createPlayer :: PlayerId -> String -> Player
createPlayer pid playerName =
  Player
    { name = playerName
    , playerId = pid
    , schicksalswesen = PlatzhalterSchicksalswesen
    , deck = []
    , hand = []
    , field = []
    , graveyard = []
    , schicksalsmacht = 0
    }

initialGameState :: GameState
initialGameState = initialState
 where
  deck1 = zipWith (\n card -> CardInPlay{id = CardId n, owner = Player1, card = card, modifications = []}) [1 ..] series26
  deck2 = zipWith (\n card -> CardInPlay{id = CardId n, owner = Player2, card = card, modifications = []}) [(last deck1).id.get + 1 ..] series26
  initialState =
    GameState
      { players =
          ( (createPlayer Player1 "player1"){deck = deck1}
          , (createPlayer Player2 "player2"){deck = deck2}
          )
      , currentPlayer = Player1
      , nextCardId = (last deck2).id.get
      }

getGameState :: State GameState :> es => Eff es GameState
getGameState = get

getsGame :: State GameState :> es => (GameState -> a) -> Eff es a
getsGame = gets

currentPlayer :: GameState -> Player
currentPlayer state = playerById state.currentPlayer state

opponentPlayer :: GameState -> Player
opponentPlayer state = playerById (otherPlayerId state.currentPlayer) state

playerByIdL :: PlayerId -> Lens' GameState Player
playerByIdL playerId = lensVL \f state -> case state.players of
  (player1, player2) -> case playerId of
    Player1 -> (\updatedPlayer -> state{players = (updatedPlayer, player2)}) <$> f player1
    Player2 -> (\updatedPlayer -> state{players = (player1, updatedPlayer)}) <$> f player2

currentPlayerL :: Lens' GameState Player
currentPlayerL = lensVL \f state ->
  case state.currentPlayer of
    Player1 -> (\updatedPlayer -> state{players = (updatedPlayer, snd state.players)}) <$> f (fst state.players)
    Player2 -> (\updatedPlayer -> state{players = (fst state.players, updatedPlayer)}) <$> f (snd state.players)

opponentPlayerL :: Lens' GameState Player
opponentPlayerL = lensVL \f state ->
  case otherPlayerId state.currentPlayer of
    Player1 -> (\updatedPlayer -> state{players = (updatedPlayer, snd state.players)}) <$> f (fst state.players)
    Player2 -> (\updatedPlayer -> state{players = (fst state.players, updatedPlayer)}) <$> f (snd state.players)

playerById :: PlayerId -> GameState -> Player
playerById playerId state = case state.players of
  (player1, player2) -> case playerId of
    Player1 -> player1
    Player2 -> player2

otherPlayerId :: PlayerId -> PlayerId
otherPlayerId = \case
  Player1 -> Player2
  Player2 -> Player1
