module GameState where

import Cards (series26)
import DataTypes
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.State.Static.Local (State, evalState, execState, get, gets, modify)

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

playerById :: PlayerId -> GameState -> Player
playerById playerId state = case state.players of
  (player1, player2) -> case playerId of
    Player1 -> player1
    Player2 -> player2

otherPlayerId :: PlayerId -> PlayerId
otherPlayerId = \case
  Player1 -> Player2
  Player2 -> Player1
