import DataTypes
import Data.Tuple
import Control.Monad.State
import Control.Lens

dragonEgg = Card "Dragon Egg" [OnTurnEnd $ AddToField dragon]
dragon = Card "Dragon" [OnPlay $ AddToField dragonEgg]

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
parseGameAction "playDragon" = Play dragon
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
resolve (AddToField c) g = over (activePlayer.field) (c:) g
resolve EndTurn g = endRound g
resolve _ g = g

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
    let player1 = Player {_name = "player1", _deck = [], _hand = [Card "test" []], _field = []}
    let player2 = Player {_name = "player2", _deck = [], _hand = [Card "test2" []], _field = []}
    let game = GameState (player1,player2)
    _ <- gameLoop game
    putStrLn "Game end"