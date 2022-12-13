module Main where

import Data.Function ((&))
import Game
import Polysemy (embed, runM)
import Polysemy.Input (runInputSem)
import Polysemy.Trace (traceToStdout)

main :: IO ()
main = do
  startGame
    & traceToStdout
    & runInputSem (embed getLine)
    & runInputSem (embed readInt)
    & runM

readInt :: IO Int
readInt = readLn