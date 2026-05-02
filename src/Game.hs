{-# LANGUAGE OverloadedRecordDot #-}

module Game where

import DataTypes
import Effectful (Eff, IOE, (:>))
import Effectful.State.Static.Local (State, evalState, gets, modify)
import GameActionParser (GameAction (..), parseGameAction)
import GameEffects (ChoiceInput, CommandInput, Log, readCommand)
import GameIO qualified as Gio
import GameState (currentPlayer, initialGameState, opponentPlayer, otherPlayerId)
import Interpreter.Game qualified as GameInterpreter

type HasStateIO es = (State GameState :> es, ChoiceInput :> es, Log :> es)
type Game es a = HasStateIO es => Eff es a

playGame :: HasStateIO r => [GameAction] -> Eff r ()
playGame = mapM_ resolveAction

gameLoop :: (CommandInput :> es, HasStateIO es) => Eff es ()
gameLoop = do
  activePlayer <- gets currentPlayer
  opponent <- gets opponentPlayer

  Gio.logLn' $ "Am Zug: " <> activePlayer.name
  Gio.logLn' $ "Gegnerische Schicksalsmacht: " <> show opponent.schicksalsmacht
  Gio.logLn' "Enemy field:"
  logField opponent

  Gio.logLn' $ "Deine Schicksalsmacht: " <> show activePlayer.schicksalsmacht
  Gio.logLn' "Your field:"
  logField activePlayer

  Gio.logLn' "Player Hand:"
  logHand activePlayer

  Gio.log' "Select action (pass/end/p/c/a/d): "
  inp <- readCommand
  if inp == "exit" || inp == "q"
    then Gio.logLn' "k bye"
    else do
      case parseGameAction inp of
        Nothing -> Gio.logLn' "Ungültige Eingabe."
        Just action -> playGame [action]
      gameLoop

startGame :: (IOE :> es, CommandInput :> es, ChoiceInput :> es, Log :> es) => Eff es ()
startGame = evalState (drawOpeningHands initialGameState) gameLoop

resolveAction :: HasStateIO r => GameAction -> Eff r ()
resolveAction = \case
  PlayFromHand index -> GameInterpreter.playCardFromHand index
  ActivateFromField index -> GameInterpreter.activateCardOnField index
  AnnounceAttack source target ->
    Gio.logLn' $ "Angriff ist noch nicht implementiert: " <> show (source + 1) <> " -> " <> show (target + 1)
  AnnounceDirectAttack source ->
    Gio.logLn' $ "Direkter Angriff ist noch nicht implementiert: " <> show (source + 1)
  Pass ->
    Gio.logLn' "Passe."
  EndRound ->
    endRound
  Play card ->
    Gio.logLn' $ "Direktes Spielen ist nicht implementiert: " <> card.name

endRound :: HasStateIO r => Eff r ()
endRound = do
  modify \state ->
    state
      { players =
          let (player1, player2) = state.players
           in ( player1{field = fmap removeTemporaryModifications player1.field}
              , player2{field = fmap removeTemporaryModifications player2.field}
              )
      , currentPlayer = otherPlayerId state.currentPlayer
      }
  Gio.logLn' "Runde beendet."

logField :: HasStateIO r => Player -> Eff r ()
logField player =
  if null player.field
    then Gio.logLn' "  (empty)"
    else Gio.displayEnumeratedItems $ renderFieldCard <$> player.field

logHand :: HasStateIO r => Player -> Eff r ()
logHand player =
  if null player.hand
    then Gio.logLn' "  (empty)"
    else Gio.displayEnumeratedItems player.hand

renderFieldCard :: CardInPlay -> String
renderFieldCard cardInPlay = case cardInPlay.card.cardType of
  Wesen _ _ ->
    cardInPlay.card.name <> " [" <> show (creatureStrength cardInPlay) <> "]"
  _ ->
    cardInPlay.card.name

creatureStrength :: CardInPlay -> Int
creatureStrength = GameInterpreter.creatureStrength

drawOpeningHands :: GameState -> GameState
drawOpeningHands = GameInterpreter.drawOpeningHands

removeTemporaryModifications :: CardInPlay -> CardInPlay
removeTemporaryModifications = GameInterpreter.removeTemporaryModifications
