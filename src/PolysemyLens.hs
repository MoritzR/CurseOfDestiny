module PolysemyLens ((+=), (-=), (++=), (%=), use) where

import Control.Lens (ASetter, Getting, view, (%~))
import Polysemy (Member, Sem)
import Polysemy.State (State, gets, modify)

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
l -= x = l %= (+ (-x))

infixl 8 ++=

(++=) :: (Member (State s) r) => ASetter s s [a] [a] -> [a] -> Sem r ()
l ++= x = l %= (++ x)

use :: (Member (State s) r) => Getting a s a -> Sem r a
use = gets . view