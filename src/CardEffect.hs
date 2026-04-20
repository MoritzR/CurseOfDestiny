{-# LANGUAGE GADTs #-}

module CardEffect where

import DataTypesNew (InstructionF(..), Wählbar(wahlmöglichkeiten), Instruction(..))

-- instruction methods
ziehe anzahl = InstructionF [Ziehe anzahl] ()
erhöhe wert ziel dauer höhe = InstructionF [Erhöhe wert ziel dauer höhe] ()
vision anzahlKarten = InstructionF [Vision anzahlKarten] ()
prisma next = InstructionF [Prisma next] ()
spende anzahl element = InstructionF [Spende anzahl element] ()
wähle :: Wählbar a => (a -> InstructionF ()) -> InstructionF ()
wähle = wähleAus wahlmöglichkeiten
opfere ziel = InstructionF [Opfere ziel] ()
heile anzahl = InstructionF [Heile anzahl] ()
gibAufDieHandZurück ziel = InstructionF [GibAufDieHandZurück ziel] ()
zerstöre ziel = InstructionF [Zerstöre ziel] ()
verringere wert ziel dauer höhe = InstructionF [Verringere wert ziel dauer höhe] ()
verringereUndZerstöre ziel dauer höhe = InstructionF [VerringereUndZerstöre ziel dauer höhe] ()
nimmAufDieHand ziel = InstructionF [NimmAufDieHand ziel] ()
zeigeObenVomDeck anzahl wert next = InstructionF [ZeigeObenVomDeck anzahl wert next] ()
beschwöre karte = InstructionF [Beschwöre karte] ()
gibFähigkeit ziel dauer fähigkeit = InstructionF [GibFähigkeit ziel dauer fähigkeit] ()
einSpielerOpfertEinWesen = InstructionF [EinSpielerOpfertEinWesen] ()
siehHandkartenAnUndEntferneEineAusDemSpiel = InstructionF [SiehHandkartenAnUndEntferneEineAusDemSpiel] ()

wähleAus :: Wählbar a => [a] -> (a -> InstructionF ()) -> InstructionF ()
wähleAus möglichkeiten next = InstructionF [Wähle möglichkeiten next] ()

