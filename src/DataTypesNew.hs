{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataTypesNew where

pattern X :: Element -> Kosten
pattern X element = Kosten [VariableElementKosten element]

data ElementKosten = ElementKosten Anzahl Element | VariableElementKosten Element | Nichts
data Element
  = Neutral
  | Feuer
  | Wald
  | Wasser
  | Wind
  | Licht
  | Tod
  | Doppel Element Element
  deriving Eq

instance Show Element where
  show = \case
    Neutral -> "⏺"
    Feuer -> "🔥"
    Wald -> "🌳"
    Wasser -> "💧"
    Wind -> "⚡"
    Licht -> "🌞"
    Tod -> "💀"
    Doppel a b -> show a <> "/" <> show b

newtype Kosten = Kosten [ElementKosten]

data CardType
  = Allmagie
  | Gegenmagie
  | Magie
  | MagieDauerhaft
  | Wesen Wesenstyp Int

data Wesenstyp
  = Konstrukt
  | Magier
  | Krieger
  | Bestie
data Card = Card
  { name :: String
  , cost :: Kosten
  , cardType :: CardType
  , trigger :: Trigger
  }

type CardEffect = InstructionF ()

data Instruction
  = Ziehe Anzahl
  | Erhöhe Wert Ziel Dauer Höhe
  | Vision Anzahl
  | Prisma (Anzahl -> InstructionF ())
  | Spende Anzahl Element
  | forall a. Wählbar a => Wähle [a] (a -> InstructionF ())
  | WähleAktion [InstructionF ()]
  | Opfere Ziel
  | Heile Anzahl
  | GibAufDieHandZurück Ziel
  | Zerstöre Ziel
  | Verringere Wert Ziel Dauer Höhe
  | VerringereUndZerstöre Ziel Dauer Höhe
  | NimmAufDieHand Ziel
  | ZeigeObenVomDeck Anzahl LesbarerWert (Höhe -> InstructionF ())
  | BringeInsSpiel Card
  | BringeInsSpielAusZiel Ziel
  | GibFähigkeit Ziel Dauer (TriggerInstructionF ())
  | EinSpielerOpfertEinWesen
  | AnzahlVon Ziel (Anzahl -> InstructionF ())
  | WirfAb Anzahl SpendetOderSpendetNicht
  | LegeVomDeckAufDenFriedhof Anzahl SpendetOderSpendetNicht
  | SchaueObenVomDeck Anzahl (InstructionWhenViewingDeckF ())
  | SiehHandkartenAnUndEntferneEineAusDemSpiel
  | BringeKopieInsSpiel Ziel
  | AnzahlSchicksalsMächte SpielerZiel (Anzahl -> InstructionF ())

data InstructionWhenViewingDeck
  = ZeigeVorUndNimmAufDieHand Ziel
  | ZeigeVorUndWirfAb Ziel
  | LegeRestUnterDasDeck
  | WähleVomDeck [InstructionWhenViewingDeckF ()]

data SpendetOderSpendetNicht = Spendet | SpendetNicht

data SpielerZiel = Du | Gegner

data Anzahl
  = PlaceHolder String
  | Actual Int
  | Mul Anzahl Anzahl
  | Add Anzahl Anzahl
  | Minus Anzahl Anzahl
  | Neg Anzahl
  deriving Eq

instance Show Anzahl where
  show = \case
    Actual i -> show i
    PlaceHolder s -> s
    Mul (PlaceHolder _) 1000 -> "X000"
    Mul a b -> show a <> " * " <> show b
    Minus a b -> show a <> " - " <> show b
    Add a b -> show a <> " + " <> show b
    Neg a -> "-" <> show a

instance Num Anzahl where
  fromInteger = Actual . fromIntegral
  (*) = Mul
  (+) = Add
  (-) = Minus
  negate = Neg
  abs = Actual . abs . anzahlToInt
  signum = error "unused"

anzahlToInt :: Anzahl -> Int
anzahlToInt = \case
  Actual i -> i
  PlaceHolder _ -> 0
  Mul a b -> anzahlToInt a * anzahlToInt b
  Add a b -> anzahlToInt a + anzahlToInt b
  Minus a b -> anzahlToInt a - anzahlToInt b
  Neg a -> negate $ anzahlToInt a

type Höhe = Anzahl
data Dauer = BisZumEndeDesZuges | Dauerhaft
data LesbarerWert = LesbarKosten
data Ort = Friedhof
data Wert = Stärke

type Trigger = TriggerInstructionF ()

data TriggerInstruction
  = AmEndeDerRunde CardEffect
  | AmBeginnDerRunde CardEffect
  | Zahle Kosten CardEffect
  | ZahleText String CardEffect
  | WennGespielt CardEffect
  | WennAufDemFeld Aura
  | EinmalProRunde CardEffect
  | BeimAngriff AngriffsPhase CardEffect
  | Blockierung
  | Doppelzerstörung
  | Lebensentzug
  | KannNichtAbwehren

data AngriffsPhase
  = ZuBeginn
  | WennNichtAbgewehrtWird

data TriggerInstructionF a = TriggerInstructionF [TriggerInstruction] a

instance Functor TriggerInstructionF where
  fmap f (TriggerInstructionF instructions a) = TriggerInstructionF instructions (f a)

instance Applicative TriggerInstructionF where
  pure = TriggerInstructionF []
  TriggerInstructionF instructions1 f <*> TriggerInstructionF instructions2 a =
    TriggerInstructionF (instructions1 <> instructions2) (f a)

data InstructionWhenViewingDeckF a = InstructionWhenViewingDeckF [InstructionWhenViewingDeck] a

instance Functor InstructionWhenViewingDeckF where
  fmap f (InstructionWhenViewingDeckF instructions a) = InstructionWhenViewingDeckF instructions (f a)

instance Applicative InstructionWhenViewingDeckF where
  pure = InstructionWhenViewingDeckF []
  InstructionWhenViewingDeckF instructions1 f <*> InstructionWhenViewingDeckF instructions2 a =
    InstructionWhenViewingDeckF (instructions1 <> instructions2) (f a)

data Aura

data Ziel = Ziel {anzahl :: ZielAnzahl, ziel :: EinZiel}
data ZielAnzahl = Ein | Eine | Alle | Undefiniert

data EinZiel = EinZiel {description :: String, filter :: Card -> Bool}

instance Show EinZiel where
  show = (.description)

instance Semigroup EinZiel where
  a <> b = EinZiel (a.description <> " " <> b.description) $ \card -> a.filter card && b.filter card

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

instance Num (Element -> Kosten) where
  fromInteger n e = Kosten [ElementKosten (fromInteger n) e]
  (+) = error "not used"
  (*) = error "not used"
  abs = error "not used"
  negate = error "not used"
  signum = error "not used"

instance Num Kosten where
  fromInteger n = Kosten [ElementKosten (fromInteger n) Neutral]
  (Kosten xs) + (Kosten ys) = Kosten $ xs ++ ys
  (*) = error "not used"
  abs = error "not used"
  negate = error "not used"
  signum = error "not used"
