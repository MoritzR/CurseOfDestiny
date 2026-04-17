{-# LANGUAGE GADTs #-}

module CardEffect
  ( CardEffect,
    Instruction (..),
    InstructionF (..),
    Wählbar (..),
    Dauer (..),
    Ziel (..),
    Wert (..),
    Anzahl,
    ziehe,
    erhöhe,
    vision,
    prisma,
    spende,
    wähle,
    wähleAus,
  )
where

import Element (Element (..))

type CardEffect = InstructionF ()

data Instruction
  = Ziehe Anzahl
  | Erhöhe Wert Ziel Dauer Höhe
  | Vision Anzahl
  | Prisma (Anzahl -> InstructionF ())
  | Spende Anzahl Element
  | forall a. Wählbar a => Wähle [a] (a -> InstructionF ())

-- instruction methods
ziehe anzahl = InstructionF [Ziehe anzahl] ()
erhöhe wert ziel dauer höhe = InstructionF [Erhöhe wert ziel dauer höhe] ()
vision anzahlKarten = InstructionF [Vision anzahlKarten] ()
prisma next = InstructionF [Prisma next] ()
spende anzahl element = InstructionF [Spende anzahl element] ()
wähle :: Wählbar a => (a -> InstructionF ()) -> InstructionF ()
wähle = wähleAus wahlmöglichkeiten

wähleAus :: Wählbar a => [a] -> (a -> InstructionF ()) -> InstructionF ()
wähleAus möglichkeiten next = InstructionF [Wähle möglichkeiten next] ()

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

class Show a => Wählbar a where
  wahlmöglichkeiten :: [a]

instance Wählbar Element where
  wahlmöglichkeiten = [Neutral, Feuer, Wald, Wasser, Wind, Licht, Tod]
