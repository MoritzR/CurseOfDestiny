import DataTypes
import Control.Monad.State
import Control.Lens

dragonEgg = Card "Dragon Egg" []
dragon = Card "Dragon" [OnPlay $ Play dragonEgg]

getPlayers :: GameState -> Players
getPlayers g = g^.players

-- activePlayer :: Functor f => (Player -> f Player) -> (GameState -> f GameState)
activePlayer :: Lens GameState GameState Player Player
activePlayer = players._1

endRound :: GameState -> GameState
endRound g = GameState (g^.players._2, g^.players._1)

pass :: GameState -> GameState
pass g = g

convertAction :: String -> GameAction
convertAction "playDragon" = Play dragon
convertAction "pass" = Pass
convertAction "end" = EndRound
convertAction _ = Pass

playGame :: GameAction -> GameState -> GameState
playGame (Play card) = playCard card
playGame EndRound = endRound
playGame Pass = pass

playCard :: Card -> GameState -> GameState
playCard card g = over (activePlayer.field) (card:) g

displayCards :: [Card] -> IO ()
displayCards cards = displayCardsH cards 0
displayCardsH :: [Card] -> Int -> IO ()
displayCardsH (c:cs) i = do 
    putStr $ (show i) ++ ": " ++ (show c) ++ "\n"
    displayCardsH cs (i + 1)
displayCardsH [] _ = putStr "\n"

gameOver :: IO ()
gameOver = do
  putStrLn "k bye"

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
            let gs' = playGame (convertAction inp) gs
            putStrLn $ show $ gs'
            gameLoop gs'

main :: IO ()
main =  do
    let player1 = Player {_name = "player1", _deck = [], _hand = [Card "test" []], _field = []}
    let player2 = Player {_name = "player2", _deck = [], _hand = [Card "test2" []], _field = []}
    let game = GameState (player1,player2)
    _ <- gameLoop game
    putStrLn "Game end"