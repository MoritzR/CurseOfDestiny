module Main where
import Game
import DataTypes (GameState(..))
import Data.Function ((&))
import Polysemy.State (evalState)
import Polysemy.Trace (traceToIO)
import Polysemy.Input (runInputSem)
import Polysemy (runM, embed)

main :: IO ()
main = do
    let player1 = createPlayer "player1"
    let player2 = createPlayer "player2"
    let gs = GameState (player1,player2)
    startGame -- Sem [Trace, Input String, Input Int]
        & traceToIO
        & runInputSem (embed getLine)
        & runInputSem (embed (fmap toInt getLine))
        & runM

toInt :: String -> Int
toInt = read