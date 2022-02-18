module PolysemyLens ((+=), (-=), (++=), (%=)) where

import Control.Lens (ASetter, (%~))
import Polysemy (Member, Sem)
import Polysemy.State (State, modify)

-- These methods are for modifying the Polysemy state with lenses
-- see https://github.com/polysemy-research/polysemy/issues/347

infix 8 %=

(%=) :: Member (State s) r => ASetter s s a b -> (a -> b) -> Sem r ()
l %= f = modify (l %~ f)

infixl 8 +=

(+=) :: (Member (State s) r, Num a) => ASetter s s a a -> a -> Sem r ()
l += x = l %= (+ x)

infixl 8 -=

(-=) :: (Member (State s) r, Num a) => ASetter s s a a -> a -> Sem r ()
l -= x = l %= (+ (- x))

infixl 8 ++=

(++=) :: (Member (State s) r) => ASetter s s [a] [a] -> [a] -> Sem r ()
l ++= x = l %= (++ x)