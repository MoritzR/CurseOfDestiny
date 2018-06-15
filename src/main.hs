import DataTypes
import Control.Monad.State
import Control.Lens
import Actions

dragonEgg = Card "Dragon Egg" []
dragon = Card "Dragon" [OnPlay $ Play dragonEgg]

endRound :: GameState -> GameState
endRound g = GameState (g^.players._2, g^.players._1)

pass :: GameState -> GameState
pass g = g

parseGameAction :: String -> GameAction
parseGameAction "playDragon" = Play dragon
parseGameAction "pass" = Pass
parseGameAction "end" = EndRound
parseGameAction _ = Pass

convertGameAction :: GameAction -> [Action]
convertGameAction (Play c) = [AddToField c]
convertGameAction _ = []

parseActions = convertGameAction . parseGameAction

playGame ::  [Action] -> GameState -> GameState
playGame [] g = endRound g
playGame (x:xs) g = playGame xs g'
                        where g' = resolve x g

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