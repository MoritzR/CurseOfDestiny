module Game where

import Data.Maybe (maybeToList)
import DataTypes
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State)
import GameActionParser (GameAction, parseGameAction)
import GameEffects (ChoiceInput, CommandInput, Log, readCommand)
import GameIO qualified as Gio
import Prelude hiding (log)

createPlayer :: String -> Player
createPlayer name = undefined
playGame :: [GameAction] -> Game r ()
playGame [] = return ()
playGame (x : xs) = do
  Gio.logLn' $ "resolving action: " ++ show x
  -- TODO
  playGame xs

gameOver :: Log :> es => Eff es ()
gameOver = Gio.logLn' "k bye"

type Game es a = HasStateIO es => Eff es a
type HasStateIO es = (State GameState :> es, ChoiceInput :> es, Log :> es)

gameLoop :: (CommandInput :> es, HasStateIO es) => Eff es ()
gameLoop = do
  Gio.logLn' ""

  Gio.logLn' "Enemy field:"
  -- TODO

  Gio.logLn' "Your field:"
  -- TODO

  Gio.logLn' "Player Hand:"
  -- TODO

  Gio.log' "Select action (pass/end/p/c/a/d): "
  inp <- readCommand
  if inp == "exit" || inp == "q"
    then gameOver
    else do
      playGame (maybeToList $ parseGameAction inp)
      gameLoop

startGame :: (CommandInput :> es, ChoiceInput :> es, Log :> es) => Eff es ()
startGame = do
  let player1 = createPlayer "player1"
  let player2 = createPlayer "player2"
  -- evalState (GameState (player1, player2)) gameLoop
  Gio.logLn' "Game end"
