import DataTypes
import Control.Monad.State
import Control.Lens

getPlayers :: GameState -> Players
getPlayers g = g^.players

getActivePlayer :: GameState -> Player
getActivePlayer g = if (g^.activePlayer == 0)
    then fst $ g^.players
    else snd $ g^.players

getNextPlayerIndex :: Int -> Int
getNextPlayerIndex = (-) 1

endRound :: GameState -> GameState
endRound g = GameState (g^.players) (getNextPlayerIndex $ g^.activePlayer)

pass :: GameState -> GameState
pass g = g

actionStringToFunction :: String -> (GameState -> GameState)
actionStringToFunction "end" = endRound
actionStringToFunction "pass" = pass
actionStringToFunction _ = id

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
    let player1 = Player "player1" [] [Card "test"]
    let player2 = Player "player2" [] [Card "test2"]
    let game = GameState (player1,player2) 0
    _ <- execStateT gameLoop game
    putStr "Game end"