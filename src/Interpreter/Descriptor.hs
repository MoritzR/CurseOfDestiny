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
import Control.Monad.Free (iter)
import Data.Foldable (fold)
import Data.List (intercalate)
import DataTypes

testRenderCard :: String
testRenderCard = intercalate "\n\n" $ describeCard <$> series26

describeCard :: Card -> String
describeCard card = unlines (card.name <> " - " <> describeKosten card.cost : describeTriggerLines card.trigger)

describeTrigger :: Trigger -> String
describeTrigger = unlines . describeTriggerLines

describeTriggerLines :: Trigger -> [String]
describeTriggerLines = iter describeTriggerInstruction . fmap (const [])

describeTriggerInstruction :: TriggerInstruction [String] -> [String]
describeTriggerInstruction = \case
  AmEndeDerRunde effect next ->
    ["Am Ende der Runde: " <> describeEffectInline effect] <> next
  AmBeginnDerRunde effect next ->
    ["Am Beginn der Runde: " <> describeEffectInline effect] <> next
  Zahle kosten effect next ->
    [describeKosten kosten <> ": " <> describeEffectInline effect] <> next
  WennGespielt effect next ->
    ["Wenn diese Karte gespielt wird: " <> describeEffectInline effect] <> next
  WennAufDemFeld _ next ->
    ["Solange diese Karte auf dem Feld ist: [Aura]"] <> next
  EinmalProRunde effect next ->
    ["Einmal pro Runde: " <> describeEffectInline effect] <> next
  Blockierung next ->
    ["Blockierung"] <> next
  Doppelzerstörung next ->
    ["Doppelzerstörung"] <> next
  KannNichtAbwehren next ->
    ["'Kann nicht abwehren'"] <> next
  Lebensentzug next ->
    ["Lebensentzug"] <> next
  Kriegsschrei next ->
    ["Kriegsschrei"] <> next
  BeimAngriff phase effect next ->
    let describePhase = \case
          ZuBeginn -> "angreift"
          WennNichtAbgewehrtWird -> "angreift und nicht abgewehrt wird"
     in ["Wenn diese Karte " <> describePhase phase <> ": " <> describeEffectInline effect] <> next

describeEffect :: CardEffect -> String
describeEffect = unlines . describeEffectLines

describeEffectInline :: CardEffect -> String
describeEffectInline = unwords . describeEffectLines

describeEffectLines :: CardEffect -> [String]
describeEffectLines = iter describeInstructionStep . fmap (const [])

describeInstruction :: Instruction next -> String
describeInstruction = \case
  Ziehe n _ ->
    "Ziehe " <> show n <> plural " Karte" n <> "."
  Erhöhe wert ziel dauer höhe _ ->
    describeZiel ziel <> " erhöht " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe <> "."
  Vision n _ ->
    "Vision " <> show n
  Prisma effectForX _ ->
    let effect = describeEffectInline $ effectForX $ PlaceHolder "X"
     in "Prisma - " <> effect <> " (X ist die Anzahl der Elemente die zum Bezahlen verwendet wurden)"
  Spende n element _ ->
    "Spende " <> show n <> " " <> show element
  WähleAus options _ _ ->
    "Wähle " <> intercalate ", " (map show options)
  WähleEffekt aktionen _ ->
    "Wähle aus:\n - " <> intercalate "\n - " (describeInstructionF <$> aktionen)
  WähleZiel ziel effectForTarget _ ->
    "Wähle " <> describeZiel ziel <> ". " <> describeEffectInline (effectForTarget $ placeholderTarget "das gewählte Ziel")
  Opfere ziel _ ->
    "Opfere " <> describeZiel ziel <> "."
  Heile n _ ->
    "Erhalte " <> show n <> " Schicksalsmacht."
  Schade n _ ->
    "Der Gegner verliert " <> show n <> " Schicksalsmacht."
  ZerstöreSchwächeres _ _ _ ->
    "Zerstöre das schwächere Wesen."
  GibInsDeck wo ziel _ ->
    "Gib " <> describeZiel ziel <> " " <> describeWoInsDeck wo <> " ins Deck des Besitzers zurück."
  GibAufDieHandZurück ziel _ ->
    "Gib " <> describeZiel ziel <> " auf die Hand des Besitzers zurück."
  Zerstöre ziel _ ->
    "Zerstöre " <> describeZiel ziel
  Verringere wert ziel dauer höhe _ ->
    describeZiel ziel <> " verringert " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe
  VerringereUndZerstöre ziel dauer höhe _ ->
    describeZiel ziel <> " verringert " <> possessive describeWert Stärke <> " " <> describeDauer dauer <> " um " <> show höhe <> ", wird sie dadurch 0, zerstöre es"
  NimmAufDieHand ziel _ ->
    "Nimm " <> describeZiel ziel <> " auf deine Hand."
  ZeigeObenVomDeck n lesbarerWert effectForX _ ->
    let effect = describeEffectInline $ effectForX $ PlaceHolder "X"
     in "Zeige die obersten " <> show n <> plural " Karte" n <> " deines Decks, " <> effect <> " (X ist die Summe der " <> describeLesbarerWert lesbarerWert <> " der gezeigten Karten)"
  BringeInsSpiel card _ ->
    "Bringe " <> card.name <> " ins Spiel."
  BringeInsSpielAusZiel ziel _ ->
    "Bringe ins Spiel: " <> describeZiel ziel
  WirfAb anzahl spendet _ -> "wirf " <> show anzahl <> " Karten von der Hand ab." <> describeSpendet spendet
  LegeVomDeckAufDenFriedhof anzahl spendet _ ->
    "Lege " <> show anzahl <> " Karten vom Deck auf den Friedhof." <> describeSpendet spendet
  GibFähigkeit ziel dauer triggerInstrs _ ->
    describeZiel ziel <> " erhält " <> describeDauer dauer <> " " <> describeGrantedTrigger triggerInstrs
  EinSpielerOpfertEinWesen _ ->
    "Ein Spieler opfert ein Wesen."
  SiehHandkartenAnUndEntferneEineAusDemSpiel _ ->
    "Sieh Handkarten an und entferne eine davon aus dem Spiel."
  AnzahlVon ziel effectForX _ ->
    describeEffectInline (effectForX $ PlaceHolder "X") <> " X ist die Anzahl von " <> describeZiel ziel <> "."
  AnzahlSchicksalsMächte spielerZiel effectForS _ ->
    let describeSpielerZiel = \case
          Du -> "deiner"
          Gegner -> "der gegnerischen"
     in describeEffectInline (effectForS $ PlaceHolder "S") <> " S ist die Anzahl " <> describeSpielerZiel spielerZiel <> " Schicksalsmächte."
  SchaueObenVomDeck anzahl next _ ->
    "Schaue dir die obersten " <> show anzahl <> " Karten deines Decks an. " <> describeWhenViewingDeckEffect next
  BringeKopieInsSpiel ziel _ ->
    "Wähle " <> describeZiel ziel <> " und bringe eine Kopie ins Spiel."

describeInstructionStep :: Instruction [String] -> [String]
describeInstructionStep instruction = describeInstruction instruction : fold instruction

describeInstructionF :: InstructionF () -> String
describeInstructionF = intercalate ", " . describeEffectLines

describeWhenViewingDeckEffect :: InstructionWhenViewingDeckF () -> String
describeWhenViewingDeckEffect = intercalate ", " . describeWhenViewingDeckSteps

describeWhenViewingDeckSteps :: InstructionWhenViewingDeckF () -> [String]
describeWhenViewingDeckSteps = iter describeWhenViewingDeckStep . fmap (const [])

describeWhenViewingDeckInstruction :: InstructionWhenViewingDeck next -> String
describeWhenViewingDeckInstruction = \case
  ZeigeVorUndNimmAufDieHand ziel _ ->
    "zeige " <> describeZiel ziel <> " offen vor und nimm es auf die Hand"
  ZeigeVorUndWirfAb ziel _ ->
    "zeige " <> describeZiel ziel <> " offen vor und wirf es ab"
  LegeRestUnterDasDeck _ ->
    "lege den Rest unter das Deck"
  WähleVomDeck aktionen _ ->
    "wähle aus:\n - " <> intercalate "\n - " (describeWhenViewingDeckEffect <$> aktionen)

describeWhenViewingDeckStep :: InstructionWhenViewingDeck [String] -> [String]
describeWhenViewingDeckStep instruction = describeWhenViewingDeckInstruction instruction : fold instruction

placeholderTarget :: String -> Ziel
placeholderTarget description =
  Ziel
    { anzahl = Undefiniert
    , ziel = EinZiel description \_ _ -> []
    }

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
  prefix (BisZu anzahl) = "bis zu " <> show anzahl <> " "

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

describeWoInsDeck :: WoInsDeck -> String
describeWoInsDeck = \case
  Oben -> "oben"
  Unten -> "unten"

plural :: String -> Anzahl -> String
plural word n
  | n == 1 = word
  | otherwise = word <> "n"

possessive :: (a -> String) -> a -> String
possessive render value = "seine " <> render value
