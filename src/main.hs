import DataTypes
import Data.Tuple
import Control.Monad.State
import Control.Lens

dragonEgg = Card "Dragon Egg" [OnTurnEnd $ AddToField dragon]
dragon = Card "Dragon" [OnPlay $ AddToField dragonEgg]
dog = Card "Dog" []
cat = Card "Cat" []
catOrDog = Card "Cat or Dog?" [OnPlay $ Choose [AddToField dog, AddToField cat]]

createPlayer :: String -> Player
createPlayer name = Player {_name = name, _deck = [], _hand = [dragon], _field = []}

endRound :: GameState -> IO GameState
endRound g = applyTurnEnds g
                >>= return . changeCurrentPlayer

changeCurrentPlayer :: GameState -> GameState
changeCurrentPlayer = over players swap

applyTurnEnds :: GameState -> IO GameState
applyTurnEnds g = playGame actions g
    where actions = concat $ fmap onTurnEndEffects $ g^..activePlayer.field.traverse.effects

pass :: GameState -> GameState
pass g = g

parseGameAction :: String -> GameAction
parseGameAction "play dragon" = Play dragon
parseGameAction "play catdog" = Play catOrDog
parseGameAction "pass" = Pass
parseGameAction "end" = EndRound
parseGameAction _ = Pass

convertGameAction :: GameAction -> [Action]
convertGameAction (Play c) = AddToField c : (onPlayEffects . view effects) c
convertGameAction EndRound = return EndTurn
convertGameAction _ = []

onPlayEffects :: [CardEffect] -> [Action]
onPlayEffects l = [a | OnPlay a <- l]

onTurnEndEffects :: [CardEffect] -> [Action]
onTurnEndEffects l = [a | OnTurnEnd a <- l]

parseActions = convertGameAction . parseGameAction

playGame ::  [Action] -> GameState -> IO GameState
playGame [] g = return g
playGame (x:xs) g = do
    putStrLn $ "resolved action: " ++ show x
    resolve x g >>= playGame xs

resolve :: Action -> GameState -> IO GameState
resolve (AddToField c) = return . over (activePlayer.field) (c:)
resolve (Choose l) = resolveChoose l
resolve EndTurn = endRound
resolve _ = return . id

resolveChoose :: [Action] -> GameState -> IO GameState
resolveChoose l gs = do
    maybeChoice <- getChoiceFromIO l
    print maybeChoice
    case maybeChoice of
        Just choice -> resolve choice gs
        Nothing -> return gs

getChoiceFromIO :: Show a => [a] -> IO (Maybe a)
getChoiceFromIO l = do
    displayEnumeratedItems l
    putStr "Choose one: "
    choice <- readLn
    if choice < 1 || choice > length l
        then return Nothing
        else return $ Just (l !! (choice -1))


displayEnumeratedItems :: Show a => [a] -> IO ()
displayEnumeratedItems = mapM_ displayTuple . zip [1..]
    where displayTuple (i, v) = putStrLn $ show i ++ ": " ++ (show v)

displayCards :: [Card] -> IO ()
displayCards cards = displayCardsH cards 0
displayCardsH :: [Card] -> Int -> IO ()
displayCardsH (c:cs) i = do 
    putStr $ (show i) ++ ": " ++ (show c) ++ "\n"
    displayCardsH cs (i + 1)
displayCardsH [] _ = putStr "\n"

gameOver :: IO ()
gameOver = putStrLn "k bye"

gameLoop :: GameState -> IO ()
gameLoop gs = do
    putStrLn $ "Current state: " ++ show gs
    putStrLn "Player Hand:"
    displayCards $ gs^.activePlayer.hand
    putStr "Select action (pass/end): "
    inp <- getLine
    if inp=="exit" || inp=="q"
        then gameOver
        else do
            playGame (parseActions inp) gs
                >>= gameLoop

main :: IO ()
main =  do
    let player1 = createPlayer "player1"
    let player2 = createPlayer "player2"
    let game = GameState (player1,player2)
    _ <- gameLoop game
    putStrLn "Game end"