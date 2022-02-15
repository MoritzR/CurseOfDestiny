module Game where
import Prelude hiding (log)
import DataTypes
import qualified Decks
import qualified Cards
import Data.Maybe (fromMaybe)
import Control.Lens ((^.), view)
import qualified GameIO as Gio
import GameActionParser (parseGameAction)
import Polysemy (Member, Members, Sem)
import Polysemy.State (evalState, State)
import qualified Polysemy.State as S
import Polysemy.Input (Input, input)
import Polysemy.Trace (Trace)
import Actions (resolve)

createPlayer :: String -> Player
createPlayer name = Player {name = name, deck = Decks.mixed, hand = Decks.mixed, field = [], playerCreature = Cards.defaultPlayerCreature}

orElsePass :: Maybe GameAction -> GameAction
orElsePass = fromMaybe Pass

convertGameAction :: GameAction -> GameState -> [Action]
convertGameAction (Play c) _ = c ^. #effects . #getOnPlay
convertGameAction (PlayFromHand i) gs = DiscardFromHand c : c ^. #effects . #getOnPlay
            where c = (gs^.activePlayer. #hand) !! i -- crashes program when i is out of range
convertGameAction (ActivateFromField i) gs = c ^. #effects . #getOnActivate
            where c = (gs^.activePlayer. #field) !! i -- crashes program when i is out of range
convertGameAction (AnnounceAttack target source) gs = [Attack targetCard sourceCard]
            where
                targetCard = (gs^.enemyPlayer. #field) !! target -- crashes program when target is out of range
                sourceCard = (gs^.activePlayer. #field) !! source -- crashes program when source is out of range
convertGameAction (AnnounceDirectAttack i) gs = return $ DirectAttack c enemyPlayer
            where c = (gs^.activePlayer. #field) !! i -- crashes program when i is out of range
convertGameAction EndRound _ = return EndTurn
convertGameAction _ _ = []

parseActions :: String -> GameState -> [Action]
parseActions = convertGameAction . orElsePass . parseGameAction

playGame :: [Action] -> Game r ()
playGame [] = return ()
playGame (x:xs) = do
    Gio.logLn' $ "resolving action: " ++ show x
    -- Gio.logLn $ "on current state: " ++ show g
    resolve x
    playGame xs

gameOver :: Member Trace r => Sem r ()
gameOver = Gio.logLn' "k bye"

gameLoop :: Member (Input String) r => Game r ()
gameLoop = do
    gs <- S.get
    Gio.logLn' ""
    Gio.logLn' "Enemy field:"
    Gio.displayEnumeratedItems $ gs^.enemyPlayer. #field
    Gio.logLn' "Your field:"
    Gio.displayEnumeratedItems $ gs^.activePlayer. #field
    Gio.logLn' "Player Hand:"
    Gio.displayEnumeratedItems $ gs^.activePlayer. #hand
    Gio.log' "Select action (pass/end/p/c/a/d): "
    inp <- input
    if inp=="exit" || inp=="q"
        then gameOver
        else do
            playGame (parseActions inp gs)
            gameLoop


startGame :: Members [Input String, Input Int, Trace] r => Sem r ()
startGame =  do
    let player1 = createPlayer "player1"
    let player2 = createPlayer "player2"
    evalState (GameState (player1,player2)) gameLoop
    Gio.logLn' "Game end"