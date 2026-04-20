{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module DataTypesNew where


pattern X :: Element -> Kosten
pattern X element = Kosten [VariableElementKosten element]

data ElementKosten = ElementKosten Int Element | VariableElementKosten Element | Nichts
data Element
  = Neutral
  | Feuer
  | Wald
  | Wasser
  | Wind
  | Licht
  | Tod
  | Doppel Element Element
  deriving (Eq, Show)

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
  | Opfere Ziel
  | Heile Anzahl
  | GibAufDieHandZurück Ziel
  | Zerstöre Ziel
  | Verringere Wert Ziel Dauer Höhe
  | VerringereUndZerstöre Ziel Dauer Höhe
  | NimmAufDieHand Ziel
  | ZeigeObenVomDeck Anzahl LesbarerWert ([Int] -> InstructionF ())
  | Beschwöre Card
  | GibFähigkeit Ziel Dauer (TriggerInstructionF ())
  | EinSpielerOpfertEinWesen
  | SiehHandkartenAnUndEntferneEineAusDemSpiel 

type Anzahl = Int
type Höhe = Int
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
  | Blockierung
  | Doppelzerstörung

data TriggerInstructionF a = TriggerInstructionF [TriggerInstruction] a

instance Functor TriggerInstructionF where
  fmap f (TriggerInstructionF instructions a) = TriggerInstructionF instructions (f a)

instance Applicative TriggerInstructionF where
  pure = TriggerInstructionF []
  TriggerInstructionF instructions1 f <*> TriggerInstructionF instructions2 a =
    TriggerInstructionF (instructions1 <> instructions2) (f a)

data Aura

data Ziel = Ziel { anzahl :: ZielAnzahl, ziel :: EinZiel }
data ZielAnzahl = Ein | Alle

data EinZiel = EinZiel { description :: String, filter :: Card -> Bool }

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

