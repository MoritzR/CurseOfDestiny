module Game where
import Prelude hiding (log)
import DataTypes
import qualified Decks
import qualified Cards
import Data.Tuple
import Data.Maybe (fromMaybe)
import Text.Read
import Control.Lens
import GameIO as Gio
import GameActionParser (parseGameAction)
import Polysemy (Member, Members, Sem)
import Polysemy.State (evalState, State)
import qualified Polysemy.State as S
import Polysemy.Input (Input, input)
import Polysemy.Trace (Trace)

type HasStateIO r = Members [State GameState, Input Int, Trace] r
type Game r a = HasStateIO r => Sem r a

createPlayer :: String -> Player
createPlayer name = Player {name = name, deck = Decks.mixed, hand = Decks.mixed, field = [], playerCreature = Cards.defaultPlayerCreature}

endRound :: Game r ()
endRound = do
    applyTurnEnds
    gs <- S.get
    S.put $ changeCurrentPlayer gs

changeCurrentPlayer :: GameState -> GameState
changeCurrentPlayer = over #players swap

applyTurnEnds :: Game r ()
applyTurnEnds = do
    gs <- S.get
    let actions = concat $ fmap onTurnEndEffects $ gs^..activePlayer . #field . traverse . #effects
    playGame actions

pass :: GameState -> GameState
pass = id

readFirstNumber :: [String] -> Maybe Int
readFirstNumber l = readMaybe . (!!1) $ l

orElsePass :: Maybe GameAction -> GameAction
orElsePass = fromMaybe Pass

convertGameAction :: GameAction -> GameState -> [Action]
convertGameAction (Play c) _ = (onPlayEffects . view #effects) c
convertGameAction (PlayFromHand i) gs = (DiscardFromHand c) : (onPlayEffects . view #effects) c
            where c = (gs^.activePlayer. #hand) !! i -- crashes program when i is out of range
convertGameAction (ActivateFromField i) gs = (onActivateEffects . view #effects) c
            where c = (gs^.activePlayer. #field) !! i -- crashes program when i is out of range
convertGameAction (AnnounceAttack target source) gs = [Attack targetCard sourceCard]
            where
                targetCard = (gs^.enemyPlayer. #field) !! target -- crashes program when target is out of range
                sourceCard = (gs^.activePlayer. #field) !! source -- crashes program when source is out of range
convertGameAction (AnnounceDirectAttack i) gs = return $ DirectAttack c (enemyPlayer)
            where c = (gs^.activePlayer. #field) !! i -- crashes program when i is out of range
convertGameAction EndRound _ = return EndTurn
convertGameAction _ _ = []

onPlayEffects :: [CardEffect] -> [Action]
onPlayEffects l = [a | OnPlay a <- l]
onTurnEndEffects :: [CardEffect] -> [Action]
onTurnEndEffects l = [a | OnTurnEnd a <- l]
onActivateEffects :: [CardEffect] -> [Action]
onActivateEffects l = [a | OnActivate a <- l]

creaturePower :: Card -> Int
creaturePower card = case card^. #cardType of
    Creature power  -> power
    Spell           -> 0 -- is a default a good idea here, or should this fail instead?

parseActions :: String -> GameState -> [Action]
parseActions = convertGameAction . orElsePass . parseGameAction

playGame :: [Action] -> Game r ()
playGame [] = return ()
playGame (x:xs) = do
    logLn' $ "resolving action: " ++ show x
    -- Gio.logLn $ "on current state: " ++ show g
    resolve x
    playGame xs

attack :: Card -> Card -> Game r ()
attack target source = case compare targetPower sourcePower of
    LT -> destroy (enemyPlayer. #field) target
    GT -> destroy (activePlayer. #field) source
    EQ -> do
        destroy (enemyPlayer. #field) target
        destroy (activePlayer. #field) source
    where (targetPower, sourcePower) = (creaturePower target, creaturePower source)

addToField :: Card -> Game r ()
addToField card = do
    gs <- S.get
    S.put $ over (activePlayer. #field) (card:) gs

destroy :: CardLens -> Card -> Game r ()
destroy cardLens card = do
    gs <- S.get
    S.put $ over cardLens (deleteFirst card) gs

directAttack :: Card -> PlayerLens -> Game r ()
directAttack _source targetPlayerLens = do
    gs <- S.get
    S.put $ over (targetPlayerLens.playerHp) (+ (-1)) gs

discardFromHand :: Card -> Game r ()
discardFromHand card = do
    gs <- S.get
    S.put $ over (activePlayer. #hand) (deleteFirst card) gs

destroyOne :: CardLens -> Game r ()
destroyOne cardLens = do
    gs <- S.get
    playGame [doDestroy cardLens gs]

draw :: PlayerLens -> Game r ()
draw playerLens = do
    gs <- S.get 
    S.put $ over (playerLens. #deck) tail . over (playerLens. #hand) ((:) $ topOfDeck playerLens gs) $ gs

resolve :: Action -> Game r ()
resolve (AddToField c) = addToField c
resolve (Choose l) = resolveChoose l
resolve (Destroy cardLens c) = destroy cardLens c
resolve (Attack target source) = attack target source
resolve (DirectAttack _source targetPlayerLens) = directAttack _source targetPlayerLens
resolve (DiscardFromHand c) = discardFromHand c
resolve (DestroyOne cardLens) = destroyOne cardLens
resolve (Draw playerLens) = draw playerLens
resolve EndTurn = endRound

topOfDeck :: PlayerLens -> GameState -> Card
topOfDeck playerLens = head . (^.playerLens. #deck)

doDestroy :: CardLens -> GameState -> Action
doDestroy cardLens = Choose . fmap (Destroy cardLens) . (^.cardLens)

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (b:bc)
    | a == b    = bc
    | otherwise = b : deleteFirst a bc

resolveChoose :: [Action] -> Game r ()
resolveChoose l = do
    maybeChoice <- chooseOne l
    logLn' . show $ maybeChoice
    case maybeChoice of
        Just choice -> resolve choice
        Nothing -> return ()

gameOver :: Member Trace r => Sem r ()
gameOver = logLn' "k bye"

gameLoop :: Member (Input String) r => Game r ()
gameLoop = do
    gs <- S.get
    logLn' ""
    -- Gio.logLn $ "Current state: " ++ show gs
    logLn' "Enemy field:"
    displayEnumeratedItems $ gs^.enemyPlayer. #field
    logLn' "Your field:"
    displayEnumeratedItems $ gs^.activePlayer. #field
    logLn' "Player Hand:"
    displayEnumeratedItems $ gs^.activePlayer. #hand
    log' "Select action (pass/end/p/c/a/d): "
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
    gameLoop
        & evalState (GameState (player1,player2))
    logLn' "Game end"