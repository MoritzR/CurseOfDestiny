module Main where

import Data.Function ((&))
import Effectful (runEff)
import Game
import GameEffects (runChoiceInputIO, runCommandInputIO, runLogToIO)

main :: IO ()
main = do
  startGame
    & runLogToIO putStrLn
    & runCommandInputIO getLine
    & runChoiceInputIO readInt
    & runEff

readInt :: IO Int
readInt = readLn
