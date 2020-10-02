module PolysemyTest where

import Polysemy ( Sem, runM, Members )
import Polysemy.State (State, evalState, put, get)
import Polysemy.Trace ( trace, traceToIO, Trace )
import Data.Function ((&))

type CountdownState = State Int


countdown :: Members [CountdownState, Trace] r => Sem r ()
countdown = do
    currentCounter <- get
    let newCounter = currentCounter - 1
    put newCounter
    trace $ "new Counter: " ++ show newCounter
    if newCounter == 0
        then trace $ "Countdown completed!"
        else countdown

runCountdownIO :: Int -> IO ()
runCountdownIO x = do
    countdown                   -- Sem [CountdownState, Output String] ()
        & evalState x           -- Sem [Output String] ()
        & traceToIO             -- Sem [] ()
        & runM                  -- IO ()