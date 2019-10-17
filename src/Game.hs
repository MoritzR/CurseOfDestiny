{-# LANGUAGE Rank2Types #-}

module Game where
import DataTypes
import qualified Decks
import qualified Cards
import Data.Tuple
import Data.List
import Data.Maybe (fromMaybe)
import Text.Read
import Control.Monad.State
import Control.Lens
import GameIO as Gio
import GameActionParser (parseGameAction)


createPlayer :: String -> Player
createPlayer name = Player {_name = name, _deck = Decks.mixed, _hand = Decks.mixed, _field = [], _playerCreature = Cards.defaultPlayerCreature}

endRound :: Gio.GameIO m => GameState -> m GameState
endRound g = applyTurnEnds g
                >>= return . changeCurrentPlayer

changeCurrentPlayer :: GameState -> GameState
changeCurrentPlayer = over players swap

applyTurnEnds :: Gio.GameIO m => GameState -> m GameState
applyTurnEnds g = playGame actions g
    where actions = concat $ fmap onTurnEndEffects $ g^..activePlayer.field.traverse.effects

pass :: GameState -> GameState
pass = id

readFirstNumber :: [String] -> Maybe Int
readFirstNumber l = readMaybe . (!!1) $ l

orElsePass :: Maybe GameAction -> GameAction
orElsePass = fromMaybe Pass

convertGameAction :: GameAction -> GameState -> [Action]
convertGameAction (Play c) _ = (onPlayEffects . view effects) c
convertGameAction (PlayFromHand i) gs = (DiscardFromHand c) : (onPlayEffects . view effects) c
            where c = (gs^.activePlayer.hand) !! i -- crashes program when i is out of range
convertGameAction (ActivateFromField i) gs = (onActivateEffects . view effects) c
            where c = (gs^.activePlayer.field) !! i -- crashes program when i is out of range
convertGameAction (AnnounceAttack target source) gs = [Attack targetCard sourceCard]
            where
                targetCard = (gs^.enemyPlayer.field) !! target -- crashes program when target is out of range
                sourceCard = (gs^.activePlayer.field) !! source -- crashes program when source is out of range
convertGameAction (AnnounceDirectAttack i) gs = return $ DirectAttack c (enemyPlayer)
            where c = (gs^.activePlayer.field) !! i -- crashes program when i is out of range
convertGameAction EndRound _ = return EndTurn
convertGameAction _ _ = []

onPlayEffects :: [CardEffect] -> [Action]
onPlayEffects l = [a | OnPlay a <- l]
onTurnEndEffects :: [CardEffect] -> [Action]
onTurnEndEffects l = [a | OnTurnEnd a <- l]
onActivateEffects :: [CardEffect] -> [Action]
onActivateEffects l = [a | OnActivate a <- l]

creaturePower :: Card -> Int
creaturePower c = head [power | Creature power <- c^.features]

parseActions :: String -> GameState -> [Action]
parseActions = convertGameAction . orElsePass . parseGameAction

playGame ::  Gio.GameIO m => [Action] -> GameState -> m GameState
playGame [] g = return g
playGame (x:xs) g = do
    Gio.logLn $ "resolving action: " ++ show x
    Gio.logLn $ "on current state: " ++ show g
    resolve x g >>= playGame xs

doAttack :: Card -> Card -> [Action]
doAttack target source = case compare targetPower sourcePower of
    LT -> [Destroy (enemyPlayer.field) target]
    GT -> [Destroy (activePlayer.field) source]
    EQ -> [Destroy (enemyPlayer.field) target, Destroy (activePlayer.field) source]
    where (targetPower, sourcePower) = (creaturePower target, creaturePower source)

resolve :: Gio.GameIO m => Action -> GameState -> m GameState
resolve (AddToField c) = return . over (activePlayer.field) (c:)
resolve (Choose l) = resolveChoose l
resolve (Destroy cardLens c) = return . over cardLens (deleteFirst c)
resolve (Attack target source) = playGame $ doAttack target source
resolve (DirectAttack _source targetPlayerLens) = return . (targetPlayerLens.playerHp -~ 1)
resolve (DiscardFromHand c) = return . over (activePlayer.hand) (deleteFirst c)
resolve (DestroyOne cardLens) = \gs -> playGame (doDestroy cardLens gs) gs
resolve (Draw playerLens) = \gs -> return . over (playerLens.deck) tail . over (playerLens.hand) ((:) $ topOfDeck playerLens gs) $ gs
resolve EndTurn = endRound
resolve _ = return . id

topOfDeck :: PlayerLens -> GameState -> Card
topOfDeck playerLens = head . (^.playerLens.deck)

doDestroy :: CardLens -> GameState -> [Action]
doDestroy cardLens = return . Choose . fmap (Destroy cardLens) . (^.cardLens)

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (b:bc)
    | a == b    = bc
    | otherwise = b : deleteFirst a bc

resolveChoose :: Gio.GameIO m => [Action] -> GameState -> m GameState
resolveChoose l gs = do
    maybeChoice <- getChoiceFromIO l
    logLn . show $ maybeChoice
    case maybeChoice of
        Just choice -> resolve choice gs
        Nothing -> return gs

getChoiceFromIO :: (Gio.GameIO m, Show a) => [a] -> m (Maybe a)
getChoiceFromIO = chooseOne

gameOver :: Gio.GameIO m => m ()
gameOver = Gio.logLn "k bye"

gameLoop :: Gio.GameIO m => GameState -> m ()
gameLoop gs = do
    Gio.logLn ""
    Gio.logLn $ "Current state: " ++ show gs
    Gio.logLn "Player Hand:"
    displayEnumeratedItems $ gs^.activePlayer.hand
    Gio.log "Select action (pass/end): "
    inp <- Gio.getLine
    if inp=="exit" || inp=="q"
        then gameOver
        else playGame (parseActions inp gs) gs
                >>= gameLoop

startGame :: Gio.GameIO m => m ()
startGame =  do
    let player1 = createPlayer "player1"
    let player2 = createPlayer "player2"
    let game = GameState (player1,player2)
    _ <- gameLoop game
    Gio.logLn "Game end"