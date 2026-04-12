module EffectfulLens ((+=), (-=), (++=), (%=), use) where

import Control.Lens (ASetter, Getting, view, (%~))
import Effectful ((:>), Eff)
import Effectful.State.Static.Local (State, gets, modify)

infix 8 %=

(%=) :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
l %= f = modify (l %~ f)

infixl 8 +=

(+=) :: (State s :> es, Num a) => ASetter s s a a -> a -> Eff es ()
l += x = l %= (+ x)

infixl 8 -=

(-=) :: (State s :> es, Num a) => ASetter s s a a -> a -> Eff es ()
l -= x = l %= (+ (-x))

infixl 8 ++=

(++=) :: (State s :> es) => ASetter s s [a] [a] -> [a] -> Eff es ()
l ++= x = l %= (++ x)

use :: (State s :> es) => Getting a s a -> Eff es a
use = gets . view
