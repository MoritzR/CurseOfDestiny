import DataTypes
import Control.Monad.State

getPlayers :: GameState -> [Player]
getPlayers g = players g

getActivePlayer :: GameState -> Player
getActivePlayer g = (players g) !! (activePlayer g)

getNextPlayerIndex :: GameState -> Int
getNextPlayerIndex g = (-) ((+) (-1) $ length $ players g) (activePlayer g)

endRound :: GameState -> GameState
endRound g = GameState (players g) (getNextPlayerIndex g)

pass :: GameState -> GameState
pass g = g

actionStringToFunction :: String -> (GameState -> GameState)
actionStringToFunction "end" = endRound
actionStringToFunction "pass" = pass

gameOver :: GameStateIO ()
gameOver = do
  lift $ putStrLn "k bye"

gameLoop :: GameStateIO ()
gameLoop = do
    lift $ putStr "Select action (pass/end): "
    inp <- lift $  getLine
    if inp=="exit" || inp=="q"
        then gameOver
        else do 
            modify $ actionStringToFunction inp
            gs' <- get
            lift $ putStrLn $ show $ gs'
            gameLoop

main :: IO ()
main =  do
    let player1 = Player 0 "player1" [] [Card "test"]
    let player2 = Player 1 "player2" [] [Card "test2"]
    let game = GameState [player1,player2] 0
    _ <- execStateT gameLoop game
    putStr "Game end"