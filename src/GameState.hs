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
      , nextCardId = (last deck2).id.get
      }

getGameState :: State GameState :> es => Eff es GameState
getGameState = get

getsGame :: State GameState :> es => (GameState -> a) -> Eff es a
getsGame = gets

currentPlayer :: GameState -> Player
currentPlayer = fst . (.players)

opponentPlayer :: GameState -> Player
opponentPlayer = snd . (.players)
