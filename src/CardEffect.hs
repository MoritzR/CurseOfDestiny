{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CardEffect where

import Control.Monad.Free (Free)
import Control.Monad.Free.Class (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DataTypes
import Interpreter.Describe (describeEffectInline, describeGrantedTrigger, describeWhenViewingDeckEffect)

$(makeFree ''Instruction)
$(makeFree ''InstructionWhenViewingDeck)

keinEffekt :: Applicative f => f ()
keinEffekt = pure ()

wähle :: Wählbar a => (Value a -> InstructionF ()) -> InstructionF ()
wähle = wähleAus wahlmöglichkeiten

class IntoTriggerValue raw where
  intoTriggerValue :: raw -> Value Trigger

instance IntoTriggerValue Trigger where
  intoTriggerValue = Concrete

instance IntoTriggerValue (Value Trigger) where
  intoTriggerValue = id

spende :: Anzahl -> Value Element -> InstructionF ()
spende = spendeValue

gibFähigkeit :: IntoTriggerValue raw => Ziel -> Dauer -> raw -> InstructionF ()
gibFähigkeit ziel dauer = gibFähigkeitValue ziel dauer . intoTriggerValue

class Monad f => WählbareAktion f where
  wähleAktion :: [f ()] -> f ()

instance WählbareAktion (Free Instruction) where
  wähleAktion effekte = wähleAus effekte \case
    Concrete effekt -> effekt
    Placeholder _ -> keinEffekt

instance WählbareAktion (Free InstructionWhenViewingDeck) where
  wähleAktion = wähleVomDeck

instance WahlOption Trigger where
  beschreibeWahl = describeGrantedTrigger
  placeholderText _ = "diese Fähigkeit"

instance WahlOption (InstructionF ()) where
  beschreibeWahl = describeEffectInline
  placeholderText _ = "diesen Effekt"

instance WahlOption (InstructionWhenViewingDeckF ()) where
  beschreibeWahl = describeWhenViewingDeckEffect
  placeholderText _ = "diesen Effekt"

legeRestUnterDeck :: InstructionWhenViewingDeckF ()
legeRestUnterDeck = legeRestUnterDasDeck

zeigeVorUndNimmtAufDieHand :: Ziel -> InstructionWhenViewingDeckF ()
zeigeVorUndNimmtAufDieHand = zeigeVorUndNimmAufDieHand

anzahlSchicksalsmächte :: SpielerZiel -> (Anzahl -> InstructionF ()) -> InstructionF ()
anzahlSchicksalsmächte = anzahlSchicksalsMächte
