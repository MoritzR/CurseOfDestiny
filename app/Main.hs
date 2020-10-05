module Main where
import Game
import Data.Function ((&))
import Polysemy.Trace (traceToIO)
import Polysemy.Input (runInputSem)
import Polysemy (runM, embed)

main :: IO ()
main = do
    startGame
        & traceToIO
        & runInputSem (embed getLine)
        & runInputSem (embed (fmap toInt getLine))
        & runM

toInt :: String -> Int
toInt = read