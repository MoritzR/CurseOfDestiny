{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Interpreter.Descriptor (
  describeCard,
  describeTrigger,
  describeTriggerLines,
  describeEffect,
  describeInstruction,
  testRenderCard,
) where

import Cards (series26)
import Control.Monad.Free (Free (Free, Pure))
import Data.List (intercalate)
import DataTypesNew

testRenderCard :: String
testRenderCard = intercalate "\n\n" $ describeCard <$> series26

describeCard :: Card -> String
describeCard card = unlines (card.name <> " - " <> describeKosten card.cost : describeTriggerLines card.trigger)

describeTrigger :: Trigger -> String
describeTrigger = unlines . describeTriggerLines

describeTriggerLines :: Trigger -> [String]
describeTriggerLines = \case
  Pure () -> []
  Free instruction -> describeTriggerInstruction instruction

describeTriggerInstruction :: TriggerInstruction (TriggerInstructionF ()) -> [String]
describeTriggerInstruction = \case
  AmEndeDerRunde effect next ->
    ["Am Ende der Runde: " <> describeEffectInline effect] <> describeTriggerLines next
  AmBeginnDerRunde effect next ->
    ["Am Beginn der Runde: " <> describeEffectInline effect] <> describeTriggerLines next
  Zahle kosten effect next ->
    [describeKosten kosten <> ": " <> describeEffectInline effect] <> describeTriggerLines next
  ZahleText text effect next ->
    [text <> ": " <> describeEffectInline effect] <> describeTriggerLines next
  WennGespielt effect next ->
    ["Wenn diese Karte gespielt wird: " <> describeEffectInline effect] <> describeTriggerLines next
  WennAufDemFeld _ next ->
    ["Solange diese Karte auf dem Feld ist: [Aura]"] <> describeTriggerLines next
  EinmalProRunde effect next ->
    ["Einmal pro Runde: " <> describeEffectInline effect] <> describeTriggerLines next
  Blockierung next ->
    ["Blockierung"] <> describeTriggerLines next
  Doppelzerstörung next ->
    ["Doppelzerstörung"] <> describeTriggerLines next
  KannNichtAbwehren next ->
    ["'Kann nicht abwehren'"] <> describeTriggerLines next
  Lebensentzug next ->
    ["Lebensentzug"] <> describeTriggerLines next
  BeimAngriff phase effect next ->
    let describePhase = \case
          ZuBeginn -> "amgreift"
          WennNichtAbgewehrtWird -> "angreift und nicht abgewehrt wird"
     in ["Wenn diese Karte " <> describePhase phase <> ": " <> describeEffectInline effect] <> describeTriggerLines next

describeEffect :: CardEffect -> String
describeEffect = unlines . describeEffectLines

describeEffectInline :: CardEffect -> String
describeEffectInline = intercalate ", " . describeEffectLines

describeEffectLines :: CardEffect -> [String]
describeEffectLines = \case
  Pure () -> []
  Free instruction -> describeInstructionStep instruction

describeInstruction :: Instruction next -> String
describeInstruction = \case
  Ziehe n _ ->
    "ziehe " <> show n <> plural " Karte" n
  Erhöhe wert ziel dauer höhe _ ->
    describeZiel ziel <> " erhöht " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe
  Vision n _ ->
    "Vision " <> show n
  Prisma effectForX _ ->
    let effect = describeEffectInline $ effectForX $ PlaceHolder "X"
     in "Prisma - " <> effect <> " (X ist die Anzahl der Elemente die zum Bezahlen verwendet wurden)"
  Spende n element _ ->
    "spende " <> show n <> " " <> show element
  WähleAus options _ _ ->
    "wähle " <> intercalate ", " (map show options)
  WähleEffekt aktionen _ ->
    "wähle aus:\n - " <> intercalate "\n - " (describeInstructionF <$> aktionen)
  Opfere ziel _ ->
    "opfere " <> describeZiel ziel
  Heile n _ ->
    "erhalte " <> show n <> " Schicksalsmacht"
  GibAufDieHandZurück ziel _ ->
    "gib " <> describeZiel ziel <> " auf die Hand des Besitzers zurück"
  Zerstöre ziel _ ->
    "zerstöre " <> describeZiel ziel
  Verringere wert ziel dauer höhe _ ->
    describeZiel ziel <> " verringert " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe
  VerringereUndZerstöre ziel dauer höhe _ ->
    describeZiel ziel <> " verringert " <> possessive describeWert Stärke <> " " <> describeDauer dauer <> " um " <> show höhe <> ", wird sie dadurch 0, zerstöre es"
  NimmAufDieHand ziel _ ->
    "nimm " <> describeZiel ziel <> " auf deine Hand"
  ZeigeObenVomDeck n lesbarerWert effectForX _ ->
    let effect = describeEffectInline $ effectForX $ PlaceHolder "X"
     in "zeige die obersten " <> show n <> plural " Karte" n <> " deines Decks, " <> effect <> " (X ist die Summe der " <> describeLesbarerWert lesbarerWert <> " der gezeigten Karten)"
  BringeInsSpiel card _ ->
    "bringe " <> card.name <> " ins Spiel"
  BringeInsSpielAusZiel ziel _ ->
    "bringe ins Spiel: " <> describeZiel ziel
  WirfAb anzahl spendet _ -> "wirf " <> show anzahl <> " Karten von der Hand ab." <> describeSpendet spendet
  LegeVomDeckAufDenFriedhof anzahl spendet _ ->
    "lege " <> show anzahl <> " Karten vom Deck auf den Friedhof." <> describeSpendet spendet
  GibFähigkeit ziel dauer triggerInstrs _ ->
    describeZiel ziel <> " erhält " <> describeGrantedTrigger triggerInstrs <> " " <> describeDauer dauer
  EinSpielerOpfertEinWesen _ ->
    "ein Spieler opfert ein Wesen"
  SiehHandkartenAnUndEntferneEineAusDemSpiel _ ->
    "sieh Handkarten an und entferne eine davon aus dem Spiel"
  AnzahlVon ziel effectForX _ ->
    "X ist die Anzahl von " <> describeZiel ziel <> ". " <> describeEffectInline (effectForX $ PlaceHolder "X")
  AnzahlSchicksalsMächte spielerZiel effectForS _ ->
    let describeSpielerZiel = \case
          Du -> "deiner"
          Gegner -> "der gegnerischen"
     in "S ist die Anzahl " <> describeSpielerZiel spielerZiel <> " Schicksalsmächte. " <> describeEffectInline (effectForS $ PlaceHolder "S")
  SchaueObenVomDeck anzahl next _ ->
    "Schaue dir die obersten " <> show anzahl <> " Karten deines Decks an. " <> describeWhenViewingDeckEffect next
  BringeKopieInsSpiel ziel _ ->
    "Wähle " <> describeZiel ziel <> " und bringe eine Kopie ins Spiel"

describeInstructionStep :: Instruction (InstructionF ()) -> [String]
describeInstructionStep instruction = describeInstruction instruction : describeEffectLines (instructionNext instruction)

instructionNext :: Instruction next -> next
instructionNext = \case
  Ziehe _ next -> next
  Erhöhe _ _ _ _ next -> next
  Vision _ next -> next
  Prisma _ next -> next
  Spende _ _ next -> next
  WähleAus _ _ next -> next
  WähleEffekt _ next -> next
  Opfere _ next -> next
  Heile _ next -> next
  GibAufDieHandZurück _ next -> next
  Zerstöre _ next -> next
  Verringere _ _ _ _ next -> next
  VerringereUndZerstöre _ _ _ next -> next
  NimmAufDieHand _ next -> next
  ZeigeObenVomDeck _ _ _ next -> next
  BringeInsSpiel _ next -> next
  BringeInsSpielAusZiel _ next -> next
  GibFähigkeit _ _ _ next -> next
  EinSpielerOpfertEinWesen next -> next
  AnzahlVon _ _ next -> next
  WirfAb _ _ next -> next
  LegeVomDeckAufDenFriedhof _ _ next -> next
  SchaueObenVomDeck _ _ next -> next
  SiehHandkartenAnUndEntferneEineAusDemSpiel next -> next
  BringeKopieInsSpiel _ next -> next
  AnzahlSchicksalsMächte _ _ next -> next

describeInstructionF :: InstructionF () -> String
describeInstructionF = intercalate ", " . describeEffectLines

describeWhenViewingDeckEffect :: InstructionWhenViewingDeckF () -> String
describeWhenViewingDeckEffect = intercalate ", " . describeWhenViewingDeckSteps

describeWhenViewingDeckSteps :: InstructionWhenViewingDeckF () -> [String]
describeWhenViewingDeckSteps = \case
  Pure () -> []
  Free instruction -> describeWhenViewingDeckInstruction instruction : describeWhenViewingDeckSteps (whenViewingDeckNext instruction)

describeWhenViewingDeckInstruction :: InstructionWhenViewingDeck (InstructionWhenViewingDeckF ()) -> String
describeWhenViewingDeckInstruction = \case
  ZeigeVorUndNimmAufDieHand ziel _ ->
    "zeige " <> describeZiel ziel <> " offen vor und nimm es auf die Hand"
  ZeigeVorUndWirfAb ziel _ ->
    "zeige " <> describeZiel ziel <> " offen vor und wirf es ab"
  LegeRestUnterDasDeck _ ->
    "lege den Rest unter das Deck"
  WähleVomDeck aktionen _ ->
    "wähle aus:\n - " <> intercalate "\n - " (describeWhenViewingDeckEffect <$> aktionen)

whenViewingDeckNext :: InstructionWhenViewingDeck next -> next
whenViewingDeckNext = \case
  ZeigeVorUndNimmAufDieHand _ next -> next
  ZeigeVorUndWirfAb _ next -> next
  LegeRestUnterDasDeck next -> next
  WähleVomDeck _ next -> next

describeSpendet :: SpendetOderSpendetNicht -> String
describeSpendet SpendetNicht = " Sie spenden keine Schicksalspunkte"
describeSpendet Spendet = ""

describeGrantedTrigger :: Trigger -> String
describeGrantedTrigger = intercalate ", " . describeTriggerLines

describeKosten :: Kosten -> String
describeKosten (Kosten kosten) = unwords $ map describeElementKosten kosten

describeElementKosten :: ElementKosten -> String
describeElementKosten = \case
  ElementKosten n element -> show n <> show element
  VariableElementKosten element -> "X" <> show element
  Nichts -> "nichts"

describeZiel :: Ziel -> String
describeZiel (Ziel zielAnzahl einZiel) =
  prefix zielAnzahl <> show einZiel
 where
  prefix Ein = "ein "
  prefix Eine = "eine "
  prefix Alle = "alle "
  prefix Undefiniert = ""

describeDauer :: Dauer -> String
describeDauer = \case
  BisZumEndeDesZuges -> "bis zum Ende des Zuges"
  Dauerhaft -> "dauerhaft"

describeWert :: Wert -> String
describeWert = \case
  Stärke -> "Stärke"

describeLesbarerWert :: LesbarerWert -> String
describeLesbarerWert = \case
  LesbarKosten -> "Kosten"

plural :: String -> Anzahl -> String
plural word n
  | n == 1 = word
  | otherwise = word <> "n"

possessive :: (a -> String) -> a -> String
possessive render value = "seine " <> render value
