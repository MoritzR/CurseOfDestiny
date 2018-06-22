import DataTypes
import Data.Tuple
import Control.Monad.State
import Control.Lens

dragonEgg = Card "Dragon Egg" [OnTurnEnd $ AddToField dragon]
dragon = Card "Dragon" [OnPlay $ AddToField dragonEgg]

createPlayer :: String -> Player
createPlayer name = Player {_name = name, _deck = [], _hand = [dragon], _field = []}

endRound :: GameState -> GameState
endRound = changeCurrentPlayer . applyTurnEnds

changeCurrentPlayer :: GameState -> GameState
changeCurrentPlayer = over players swap

applyTurnEnds :: GameState -> GameState
applyTurnEnds g = playGame actions g
    where actions = concat $ fmap onTurnEndEffects $ g^..activePlayer.field.traverse.effects

pass :: GameState -> GameState
pass g = g

parseGameAction :: String -> GameAction
parseGameAction "play dragon" = Play dragon
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

playGame ::  [Action] -> GameState -> GameState
playGame [] g = g
playGame (x:xs) g = playGame xs g'
                        where g' = resolve x g

resolve :: Action -> GameState -> GameState
resolve (AddToField c) = over (activePlayer.field) (c:)
resolve EndTurn = endRound
resolve _ = id

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
gameLoop game = do
    let gs = game
    putStrLn "Player Hand:"
    displayCards $ gs^.activePlayer.hand
    putStr "Select action (pass/end): "
    inp <- getLine
    if inp=="exit" || inp=="q"
        then gameOver
        else do 
            let gs' = playGame (parseActions inp) gs
            putStrLn $ show $ gs'
            gameLoop gs'

main :: IO ()
main =  do
    let player1 = createPlayer "player1"
    let player2 = createPlayer "player2"
    let game = GameState (player1,player2)
    _ <- gameLoop game
    putStrLn "Game end"