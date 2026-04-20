{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Interpreter.Descriptor
  ( describeCard
  , describeTrigger
  , describeTriggerLines
  , describeEffect
  , describeInstruction
  , testRenderCard
  ) where

import Data.List (intercalate)
import DataTypesNew
import Cards (series26)

testRenderCard :: String
testRenderCard = intercalate "\n\n" $ describeCard <$> series26

describeCard :: Card -> String
describeCard card = unlines (card.name : describeTriggerLines card.trigger)

describeTrigger :: Trigger -> String
describeTrigger = unlines . describeTriggerLines

describeTriggerLines :: Trigger -> [String]
describeTriggerLines (TriggerInstructionF instructions _) =
  concatMap describeTriggerInstruction instructions

describeTriggerInstruction :: TriggerInstruction -> [String]
describeTriggerInstruction = \case
  AmEndeDerRunde effect ->
    ["Am Ende der Runde: " <> describeEffectInline effect]
  AmBeginnDerRunde effect ->
    ["Am Beginn der Runde: " <> describeEffectInline effect]
  Zahle kosten effect ->
    [describeKosten kosten <> ": " <> describeEffectInline effect]
  ZahleText text effect ->
    [text <> ": " <> describeEffectInline effect]
  WennGespielt effect ->
    ["Wenn diese Karte gespielt wird: " <> describeEffectInline effect]
  WennAufDemFeld _ ->
    ["Solange diese Karte auf dem Feld ist: [Aura]"]
  EinmalProRunde effect ->
    ["Einmal pro Runde: " <> describeEffectInline effect]
  Blockierung ->
    ["Blockierung"]
  Doppelzerstörung ->
    ["Doppelzerstörung"]

describeEffect :: CardEffect -> String
describeEffect = unlines . describeEffectLines

describeEffectInline :: CardEffect -> String
describeEffectInline = intercalate ", " . describeEffectLines

describeEffectLines :: CardEffect -> [String]
describeEffectLines (InstructionF instructions _) =
  map describeInstruction instructions

describeInstruction :: Instruction -> String
describeInstruction = \case
  Ziehe n ->
    "ziehe " <> show n <> plural " Karte" n
  Erhöhe wert ziel dauer höhe ->
    describeZiel ziel <> " erhöht " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe
  Vision n ->
    "Vision " <> show n
  Prisma _ ->
    "Prisma"
  Spende n element ->
    "spende " <> show n <> " " <> pluralizedElement element n
  Wähle options _ ->
    "wähle " <> intercalate ", " (map show options)
  Opfere ziel ->
    "opfere " <> describeZiel ziel
  Heile n ->
    "erhalte " <> show n <> " Schicksalsmacht"
  GibAufDieHandZurück ziel ->
    "gib " <> describeZiel ziel <> " auf die Hand des Besitzers zurück"
  Zerstöre ziel ->
    "zerstöre " <> describeZiel ziel
  Verringere wert ziel dauer höhe ->
    describeZiel ziel <> " verringert " <> possessive describeWert wert <> " " <> describeDauer dauer <> " um " <> show höhe
  VerringereUndZerstöre ziel dauer höhe ->
    describeZiel ziel <> " verringert " <> possessive describeWert Stärke <> " " <> describeDauer dauer <> " um " <> show höhe <> ", wird sie dadurch 0, zerstöre es"
  NimmAufDieHand ziel ->
    "nimm " <> describeZiel ziel <> " auf deine Hand"
  ZeigeObenVomDeck n lesbarerWert _ ->
    "zeige die obersten " <> show n <> plural " Karte" n <> " deines Decks; der folgende Effekt hängt von " <> describeLesbarerWert lesbarerWert <> " ab"
  Beschwöre card ->
    "beschwöre " <> card.name
  GibFähigkeit ziel dauer triggerInstrs ->
    describeZiel ziel <> " erhält " <> describeGrantedTrigger triggerInstrs <> " " <> describeDauer dauer
  EinSpielerOpfertEinWesen ->
    "ein Spieler opfert ein Wesen"
  SiehHandkartenAnUndEntferneEineAusDemSpiel ->
    "sieh Handkarten an und entferne eine davon aus dem Spiel"

describeGrantedTrigger :: Trigger -> String
describeGrantedTrigger (TriggerInstructionF instructions _) =
  intercalate ", " (concatMap describeTriggerInstruction instructions)

describeKosten :: Kosten -> String
describeKosten (Kosten kosten) =
  intercalate " + " (map describeElementKosten kosten)

describeElementKosten :: ElementKosten -> String
describeElementKosten = \case
  ElementKosten n element -> show n <> " " <> pluralizedElement element n
  VariableElementKosten element -> "X " <> show element
  Nichts -> "nichts"

describeZiel :: Ziel -> String
describeZiel (Ziel zielAnzahl einZiel) =
  prefix zielAnzahl <> show einZiel
  where
    prefix Ein = ""
    prefix Alle = "alle "

describeDauer :: Dauer -> String
describeDauer = \case
  BisZumEndeDesZuges -> "bis zum Ende des Zuges"
  Dauerhaft -> "dauerhaft"

describeWert :: Wert -> String
describeWert = \case
  Stärke -> "Stärke"

describeLesbarerWert :: LesbarerWert -> String
describeLesbarerWert = \case
  LesbarKosten -> "den Kosten"

plural :: String -> Int -> String
plural word n
  | n == 1 = word
  | otherwise = word <> "n"

pluralizedElement :: Element -> Int -> String
pluralizedElement element n
  | n == 1 = show element
  | otherwise = show element <> "-Elemente"

possessive :: (a -> String) -> a -> String
possessive render value = "seine " <> render value
