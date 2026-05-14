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

wähle :: Wählbar a => (a -> InstructionF ()) -> InstructionF ()
wähle = wähleAus wahlmöglichkeiten

class Monad f => WählbareAktion f where
  wähleAktion :: [f ()] -> f ()

instance WählbareAktion (Free Instruction) where
  wähleAktion effekte = wähleAus effekte id

instance WählbareAktion (Free InstructionWhenViewingDeck) where
  wähleAktion = wähleVomDeck

instance WahlOption Trigger where
  beschreibeWahl = describeGrantedTrigger

instance WahlOption (InstructionF ()) where
  beschreibeWahl = describeEffectInline

instance WahlOption (InstructionWhenViewingDeckF ()) where
  beschreibeWahl = describeWhenViewingDeckEffect

legeRestUnterDeck :: InstructionWhenViewingDeckF ()
legeRestUnterDeck = legeRestUnterDasDeck

zeigeVorUndNimmtAufDieHand :: Ziel -> InstructionWhenViewingDeckF ()
zeigeVorUndNimmtAufDieHand = zeigeVorUndNimmAufDieHand

anzahlSchicksalsmächte :: SpielerZiel -> (Anzahl -> InstructionF ()) -> InstructionF ()
anzahlSchicksalsmächte = anzahlSchicksalsMächte
