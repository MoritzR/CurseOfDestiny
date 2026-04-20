module Trigger where

import DataTypesNew (CardEffect, Kosten, TriggerInstructionF(..), TriggerInstruction(..))


-- instruction methods
amEndeDerRunde effekt = TriggerInstructionF [AmEndeDerRunde effekt] ()
amBeginnDerRunde effekt = TriggerInstructionF [AmBeginnDerRunde effekt] ()
zahle kosten effekt = TriggerInstructionF [Zahle kosten effekt] ()
zahleText kosten effekt = TriggerInstructionF [ZahleText kosten effekt] ()
wennGespielt effekt = TriggerInstructionF [WennGespielt effekt] ()
wennAufDemFeld aura = TriggerInstructionF [WennAufDemFeld aura] ()
einmalProRunde effekt = TriggerInstructionF [EinmalProRunde effekt] ()
blockierung = TriggerInstructionF [Blockierung] ()
doppelZerstörung = TriggerInstructionF [Doppelzerstörung] ()

-- end instruction methods

