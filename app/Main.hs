module Main where
import Game
import Data.Function ((&))
import Polysemy.Trace (traceToStdout)
import Polysemy.Input (runInputSem)
import Polysemy (runM, embed)

main :: IO ()
main = do
    startGame
        & traceToStdout
        & runInputSem (embed getLine)
        & runInputSem (embed readInt)
        & runM

readInt :: IO Int
readInt = readLn