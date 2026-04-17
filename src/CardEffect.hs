module CardEffect (CardEffect, Dauer(..), Ziel(..), Wert(..), Anzahl, ziehe, erhöhe, vision, prisma) where

type CardEffect = InstructionF ()

data Instruction
  = Ziehe Anzahl
  | Erhöhe Wert Ziel Dauer Höhe
  | Vision Anzahl
  | Prisma (Anzahl -> InstructionF ())

-- instruction methods
ziehe anzahl = InstructionF [Ziehe anzahl] ()
erhöhe wert ziel dauer höhe = InstructionF [Erhöhe wert ziel dauer höhe] ()
vision anzahlKarten = InstructionF [Vision anzahlKarten] ()
prisma next = InstructionF [Prisma next] ()
-- end instruction methods
type Anzahl = Int
type Höhe = Int
data Dauer = BisZumEndeDesZuges | Dauerhaft
data Ziel = Selbst | EinAnderesWesen
data Wert = Stärke 
-- boilerplate

data InstructionF a = InstructionF [Instruction] a

instance Functor InstructionF where
  fmap f (InstructionF instructions a) = InstructionF instructions (f a)

instance Applicative InstructionF where
  pure = InstructionF []
  InstructionF instructions1 f <*> InstructionF instructions2 a =
    InstructionF (instructions1 <> instructions2) (f a)
