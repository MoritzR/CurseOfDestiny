module EffectfulLens ((+=), (-=), (++=), (%=), use) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets, modify)
import Optics (A_Getter, A_Setter, Is, Optic', over, view)

infix 8 %=

(%=) :: (State s :> es, Is k A_Setter) => Optic' k is s a -> (a -> a) -> Eff es ()
l %= f = modify (over l f)

infixl 8 +=

(+=) :: (State s :> es, Is k A_Setter, Num a) => Optic' k is s a -> a -> Eff es ()
l += x = l %= (+ x)

infixl 8 -=

(-=) :: (State s :> es, Is k A_Setter, Num a) => Optic' k is s a -> a -> Eff es ()
l -= x = l %= (+ (-x))

infixl 8 ++=

(++=) :: (State s :> es, Is k A_Setter) => Optic' k is s [a] -> [a] -> Eff es ()
l ++= x = l %= (++ x)

use :: (State s :> es, Is k A_Getter) => Optic' k is s a -> Eff es a
use = gets . view
