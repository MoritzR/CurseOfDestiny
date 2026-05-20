{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataTypes where

import Control.Monad.Free (Free)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersect)
import GHC.Generics (Generic)
import Optics (Traversal', adjoin, both, traversed, (%))

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
  | Ausrüstung
  | MagieDauerhaft
  | Wesen Int
  deriving (Eq, Show)

data Tag
  = Konstrukt
  | Magier
  | Krieger
  | Bestie
  | Geist
  | Archon
  | Magiestein
  deriving (Eq, Show)

data Card = Card
  { name :: String
  , cost :: Kosten
  , cardType :: CardType
  , trigger :: Trigger
  , tags :: [Tag]
  }
instance Show Card where
  show = (.name)
instance Eq Card where
  a == b = a.name == b.name

data SpendetOderSpendetNicht = Spendet | SpendetNicht

data SpielerZiel = Du | Gegner
  deriving (Eq, Show)

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
  deriving (Eq, Show)
data LesbarerWert = LesbarKosten
  deriving (Eq, Show)
data Ort = Friedhof
data Wert = Stärke
  deriving (Eq, Show)

data AngriffsPhase
  = ZuBeginn
  | WennNichtAbgewehrtWird
  deriving (Eq, Show)

data Aura

data Ziel = Ziel {anzahl :: ZielAnzahl, ziel :: EinZiel}
data ZielAnzahl = Ein | Eine | Alle | Undefiniert | BisZu Anzahl

data EinZiel = EinZiel {description :: String, candidates :: GameState -> CardId -> [CardInPlay] -> [CardInPlay]}

instance Show EinZiel where
  show = (.description)

instance Semigroup EinZiel where
  a <> b =
    EinZiel (a.description <> " " <> b.description) $ \state sourceId availableCards ->
      a.candidates state sourceId availableCards `intersect` b.candidates state sourceId availableCards

data TriggerInstruction next
  = AmEndeDerRunde CardEffect next
  | AmBeginnDerRunde CardEffect next
  | AmBeginnDerKampfPhase CardEffect next
  | Zahle Kosten CardEffect next
  | WennGespielt CardEffect next
  | WennAufDemFeld Aura next
  | EinmalProRunde CardEffect next
  | BeimAngriff AngriffsPhase CardEffect next
  | Blockierung next
  | Doppelzerstörung next
  | Doppelangriff next
  | Lebensentzug next
  | KannNichtAbwehren next
  | Kriegsschrei next
  | Ermächtigung Kosten (Anzahl -> CardEffect) next
  | WirdZielVon Ziel CardEffect next
  deriving (Functor, Foldable)

data InstructionWhenViewingDeck next
  = ZeigeVorUndNimmAufDieHand Ziel next
  | ZeigeVorUndWirfAb Ziel next
  | LegeRestUnterDasDeck next
  | LegeRestAufDenFriedhof SpendetOderSpendetNicht next
  | WähleVomDeck [InstructionWhenViewingDeckF ()] next
  deriving (Functor, Foldable)

data Value a = Concrete a | Placeholder String deriving Foldable

data Instruction next
  = Ziehe Anzahl next
  | Erhöhe Wert Ziel Dauer Höhe next
  | Vision Anzahl next
  | Prisma (Anzahl -> CardEffect) next
  | SpendeValue Anzahl (Value Element) next
  | forall a. WahlOption a => WähleAus [a] (Value a -> CardEffect) next
  | WähleZiel Ziel (Ziel -> CardEffect) next
  | Opfere Ziel next
  | GegnerOpfert Ziel next
  | Heile Anzahl next
  | Schade Anzahl next
  | ZerstöreSchwächeres Ziel Ziel next
  | GibInsDeck WoInsDeck Ziel next
  | GibAufDieHandZurück Ziel next
  | Zerstöre Ziel next
  | EntferneAusDemSpiel Ziel next
  | Verringere Wert Ziel Dauer Höhe next
  | VerringereUndZerstöre Ziel Dauer Höhe next
  | NimmAufDieHand Ziel next
  | ZeigeObenVomDeck Anzahl LesbarerWert (Höhe -> CardEffect) next
  | BringeInsSpiel Anzahl Card next
  | BringeInsSpielAusZiel Ziel next
  | GibFähigkeitValue Ziel Dauer (Value (TriggerInstructionF ())) next
  | EinSpielerOpfertEinWesen next
  | AnzahlVon Ziel (Anzahl -> CardEffect) next
  | WirfAb Anzahl SpendetOderSpendetNicht next
  | GegnerWirfAb Anzahl SpendetOderSpendetNicht next
  | LegeVomDeckAufDenFriedhof Anzahl SpendetOderSpendetNicht next
  | SchaueObenVomDeck Anzahl (InstructionWhenViewingDeckF ()) next
  | SiehHandkartenAnUndEntferneEineAusDemSpiel next
  | BringeKopieInsSpiel Ziel next
  | AnzahlSchicksalsMächte SpielerZiel (Anzahl -> CardEffect) next

deriving instance Functor Instruction
deriving instance Foldable Instruction

data WoInsDeck = Oben | Unten | AnPosition Int

type TriggerInstructionF = Free TriggerInstruction
type Trigger = TriggerInstructionF ()

type InstructionWhenViewingDeckF = Free InstructionWhenViewingDeck

type InstructionF = Free Instruction
type CardEffect = InstructionF ()

class WahlOption a where
  beschreibeWahl :: a -> String
  placeholderText :: String

class WahlOption a => Wählbar a where
  wahlmöglichkeiten :: [a]

instance Wählbar Element where
  wahlmöglichkeiten = [Neutral, Feuer, Wald, Wasser, Wind, Licht, Tod]

instance WahlOption Element where
  beschreibeWahl = show
  placeholderText = "dieses Elements"

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

data Schicksalswesen = PlatzhalterSchicksalswesen
  deriving (Eq, Show)

data Modification
  = StärkeModifikation Dauer Int
  | FähigkeitsModifikation Dauer Trigger

instance Eq Modification where
  StärkeModifikation dauer delta == StärkeModifikation otherDauer otherDelta =
    dauer == otherDauer && delta == otherDelta
  FähigkeitsModifikation dauer _ == FähigkeitsModifikation otherDauer _ =
    dauer == otherDauer
  _ == _ = False

instance Show Modification where
  show = \case
    StärkeModifikation dauer delta -> "StärkeModifikation " <> show dauer <> " " <> show delta
    FähigkeitsModifikation dauer _ -> "FähigkeitsModifikation " <> show dauer

data PlayerId = Player1 | Player2
  deriving (Eq, Show)

data Player = Player
  { name :: String
  , playerId :: PlayerId
  , schicksalswesen :: Schicksalswesen
  , deck :: [CardInPlay]
  , hand :: [CardInPlay]
  , field :: [CardInPlay]
  , graveyard :: [CardInPlay]
  , schicksalsmacht :: Int
  }
  deriving (Eq, Show, Generic)

newtype CardId = CardId {get :: Int} deriving (Eq, Show)

data CardInPlay = CardInPlay
  { id :: CardId
  , owner :: PlayerId
  , card :: Card
  , modifications :: [Modification]
  }
  deriving (Show, Generic)
instance Eq CardInPlay where
  a == b = a.id == b.id

data GameState = GameState
  { players :: (Player, Player)
  , currentPlayer :: PlayerId
  , nextCardId :: Int
  }
  deriving (Eq, Show, Generic)

allCards :: Traversal' GameState CardInPlay
allCards = #players % both % playerCards
 where
  playerCards =
    [#field, #deck, #hand, #graveyard]
      <&> (% traversed)
      & foldr1 adjoin

fieldCards :: Traversal' GameState CardInPlay
fieldCards = #players % both % #field % traversed
