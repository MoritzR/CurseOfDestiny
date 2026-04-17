module Trigger where

import CardEffect (CardEffect)
import Element (Kosten)

type Trigger = TriggerInstructionF ()

data Aura

data TriggerInstruction
  = AmEndeDerRunde CardEffect
  | AmBeginnDerRunde CardEffect
  | Zahle Kosten CardEffect
  | WennGespielt CardEffect
  | WennAufDemFeld Aura

-- instruction methods
amEndeDerRunde effekt = TriggerInstructionF [AmEndeDerRunde effekt] ()
amBeginnDerRunde effekt = TriggerInstructionF [AmBeginnDerRunde effekt] ()
zahle kosten effekt = TriggerInstructionF [Zahle kosten effekt] ()
wennGespielt effekt = TriggerInstructionF [WennGespielt effekt] ()
wennAufDemFeld aura = TriggerInstructionF [WennAufDemFeld aura] ()

-- end instruction methods

data TriggerInstructionF a = TriggerInstructionF [TriggerInstruction] a

instance Functor TriggerInstructionF where
  fmap f (TriggerInstructionF instructions a) = TriggerInstructionF instructions (f a)

instance Applicative TriggerInstructionF where
  pure = TriggerInstructionF []
  TriggerInstructionF instructions1 f <*> TriggerInstructionF instructions2 a =
    TriggerInstructionF (instructions1 <> instructions2) (f a)
