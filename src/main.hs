import DataTypes
import Control.Monad.State
import Control.Lens

getPlayers :: GameState -> Players
getPlayers g = g^.players

getActivePlayer :: GameState -> Player
getActivePlayer g = fst $ g^.players


endRound :: GameState -> GameState
endRound g = GameState (snd $ g^.players, fst $ g^.players)

pass :: GameState -> GameState
pass g = g


actionStringToFunction :: String -> (GameState -> GameState)
actionStringToFunction "end" = endRound
actionStringToFunction "pass" = pass
actionStringToFunction _ = id


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
    displayCards $ (getActivePlayer gs)^.hand
    putStr "Select action (pass/end): "
    inp <- getLine
    if inp=="exit" || inp=="q"
        then gameOver
        else do 
            let gs' = actionStringToFunction inp gs
            putStrLn $ show $ gs'
            gameLoop gs'

main :: IO ()
main =  do
    let player1 = Player "player1" [] [Card "test"]
    let player2 = Player "player2" [] [Card "test2"]
    let game = GameState (player1,player2)
    _ <- gameLoop game
    putStrLn "Game end"