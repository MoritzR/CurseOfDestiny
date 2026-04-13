module CardEffect (CardEffect, Dauer(..), Anzahl, ziehe, erhöheStärke) where

type CardEffect = InstructionF ()

data Instruction
  = Ziehe Anzahl
  | ErhöheStärke Dauer

-- instruction methods
ziehe anzahl = InstructionF [Ziehe anzahl] ()
erhöheStärke dauer = InstructionF [ErhöheStärke dauer] ()
-- end instruction methods

type Anzahl = Int
data Dauer = BisZumEndeDerRunde | Dauerhaft

data InstructionF a = InstructionF [Instruction] a

instance Functor InstructionF where
  fmap f (InstructionF instructions a) = InstructionF instructions (f a)

instance Applicative InstructionF where
  pure = InstructionF []
  InstructionF instructions1 f <*> InstructionF instructions2 a =
    InstructionF (instructions1 <> instructions2) (f a)
