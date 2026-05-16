module Trigger where

import Control.Monad.Free.Class (liftF)
import DataTypes

amEndeDerRunde :: CardEffect -> Trigger
amEndeDerRunde effect = liftF $ AmEndeDerRunde effect ()

amBeginnDerRunde :: CardEffect -> Trigger
amBeginnDerRunde effect = liftF $ AmBeginnDerRunde effect ()

amBeginnDerKampfPhase :: CardEffect -> Trigger
amBeginnDerKampfPhase effect = liftF $ AmBeginnDerKampfPhase effect ()

zahle :: Kosten -> CardEffect -> Trigger
zahle kosten effect = liftF $ Zahle kosten effect ()

wennGespielt :: CardEffect -> Trigger
wennGespielt effect = liftF $ WennGespielt effect ()

wennAufDemFeld :: Aura -> Trigger
wennAufDemFeld aura = liftF $ WennAufDemFeld aura ()

einmalProRunde :: CardEffect -> Trigger
einmalProRunde effect = liftF $ EinmalProRunde effect ()

beimAngriff :: AngriffsPhase -> CardEffect -> Trigger
beimAngriff phase effect = liftF $ BeimAngriff phase effect ()

blockierung :: Trigger
blockierung = liftF $ Blockierung ()

doppelzerstörung :: Trigger
doppelzerstörung = liftF $ Doppelzerstörung ()

doppelangriff :: Trigger
doppelangriff = liftF $ Doppelangriff ()

lebensentzug :: Trigger
lebensentzug = liftF $ Lebensentzug ()

kannNichtAbwehren :: Trigger
kannNichtAbwehren = liftF $ KannNichtAbwehren ()

kriegsschrei :: Trigger
kriegsschrei = liftF $ Kriegsschrei ()

ermächtigung :: Kosten -> (Anzahl -> CardEffect) -> Trigger
ermächtigung kosten effectForX = liftF $ Ermächtigung kosten effectForX ()

wirdZielVon :: Ziel -> CardEffect -> Trigger
wirdZielVon ziel effect = liftF $ WirdZielVon ziel effect ()
