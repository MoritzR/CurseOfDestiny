{-# LANGUAGE TemplateHaskell #-}

module CardEffect where

import Control.Monad.Free (Free)
import Control.Monad.Free.Class (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DataTypesNew

$(makeFree ''Instruction)
$(makeFree ''InstructionWhenViewingDeck)

keinEffekt :: Applicative f => f ()
keinEffekt = pure ()

wähle :: Wählbar a => (a -> InstructionF ()) -> InstructionF ()
wähle = wähleAus wahlmöglichkeiten

wähleAktion :: WählbareAktion f => [f ()] -> f ()
wähleAktion = wähleAktionen

class Monad f => WählbareAktion f where
  wähleAktionen :: [f ()] -> f ()

instance WählbareAktion (Free Instruction) where
  wähleAktionen = wähleEffekt

instance WählbareAktion (Free InstructionWhenViewingDeck) where
  wähleAktionen = wähleVomDeck

legeRestUnterDeck :: InstructionWhenViewingDeckF ()
legeRestUnterDeck = legeRestUnterDasDeck

zeigeVorUndNimmtAufDieHand :: Ziel -> InstructionWhenViewingDeckF ()
zeigeVorUndNimmtAufDieHand = zeigeVorUndNimmAufDieHand

anzahlSchicksalsmächte :: SpielerZiel -> (Anzahl -> InstructionF ()) -> InstructionF ()
anzahlSchicksalsmächte = anzahlSchicksalsMächte
