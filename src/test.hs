import Control.Monad.State.Lazy
import DataTypes

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


player1 = Player 0 "player1" [] [Card "test"]
player2 = Player 1 "player2" [] [Card "test2"]
game = GameState [player1,player2] 0

type Cod game = State game

main =  do
    putStr "Select action (pass/end): "
    inp <- getLine
    if inp=="exit" || inp=="q"
        then putStrLn "bye"
        else do 
            let newG = actionStringToFunction inp game
            putStrLn $ show $ newG
            main
