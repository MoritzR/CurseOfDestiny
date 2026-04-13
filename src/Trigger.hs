module Trigger where

import CardEffect (CardEffect)

type Trigger = TriggerInstructionF ()

data Aura
data Kosten

data TriggerInstruction
  = AmEndeDerRunde CardEffect
  | Zahle Kosten CardEffect
  | WennGespielt CardEffect
  | WennAufDemFeld Aura

-- instruction methods
amEndeDerRunde effekt = TriggerInstructionF [AmEndeDerRunde effekt] ()
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
