module Trigger where

import DataTypesNew (TriggerInstruction (..), TriggerInstructionF (..))

amEndeDerRunde effekt = TriggerInstructionF [AmEndeDerRunde effekt] ()
amBeginnDerRunde effekt = TriggerInstructionF [AmBeginnDerRunde effekt] ()
zahle kosten effekt = TriggerInstructionF [Zahle kosten effekt] ()
zahleText kosten effekt = TriggerInstructionF [ZahleText kosten effekt] ()
wennGespielt effekt = TriggerInstructionF [WennGespielt effekt] ()
wennAufDemFeld aura = TriggerInstructionF [WennAufDemFeld aura] ()
einmalProRunde effekt = TriggerInstructionF [EinmalProRunde effekt] ()
beimAngriff phase effekt = TriggerInstructionF [BeimAngriff phase effekt] ()
blockierung = TriggerInstructionF [Blockierung] ()
doppelZerstörung = TriggerInstructionF [Doppelzerstörung] ()
lebensentzug = TriggerInstructionF [Lebensentzug] ()
kannNichtAbwehren = TriggerInstructionF [KannNichtAbwehren] ()
