module Cards where

import CardEffect
import DataTypes
import Target
import Trigger

mkWesen :: String -> Kosten -> Wesenstyp -> Int -> Trigger -> Card
mkWesen name cost wesenstyp stärke trigger =
  Card
    { name = name
    , cardType = Wesen wesenstyp stärke
    , cost = cost
    , trigger = trigger
    }

mkMagie :: String -> Kosten -> Trigger -> Card
mkMagie name cost trigger =
  Card
    { name = name
    , cardType = Magie
    , cost = cost
    , trigger = trigger
    }

mkGegenmagie :: String -> Kosten -> Trigger -> Card
mkGegenmagie name cost trigger =
  Card
    { name = name
    , cardType = Gegenmagie
    , cost = cost
    , trigger = trigger
    }

mkAllmagie :: String -> Kosten -> Trigger -> Card
mkAllmagie name cost trigger =
  Card
    { name = name
    , cardType = Allmagie
    , cost = cost
    , trigger = trigger
    }

mkMagieDauerHaft :: String -> Kosten -> Trigger -> Card
mkMagieDauerHaft name cost trigger =
  Card
    { name = name
    , cardType = MagieDauerhaft
    , cost = cost
    , trigger = trigger
    }

mkAusrüstung :: String -> Kosten -> Trigger -> Card
mkAusrüstung name cost trigger =
  Card
    { name = name
    , cardType = Ausrüstung
    , cost = cost
    , trigger = trigger
    }

-- tokens
schirmBestie =
  mkWesen "Schirmbestie" (2 Wald) Bestie 4000 keinEffekt

faehrgeist =
  mkWesen "Fährgeist" (1 Tod + 1 Licht) Geist 1000 lebensentzug

-- series
series26 =
  [ mkWesen "Edors Konstruct" (1 Neutral) Konstrukt 1000 $
      zahle (5 Neutral) do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 1000
  , mkAllmagie "Energieladung" (2 Neutral) $
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 2000
  , mkMagieDauerHaft "Fehrens Obelisk" (3 Neutral) do
      wennGespielt do
        ziehe 1
      zahle (5 Neutral) do
        vision 1
  , mkWesen "Forscher der Royalen Akademie" (4 Neutral) Magier 2000 do
      wennGespielt do
        vision 3
  , mkWesen "Hemtaras Krieger" (X Neutral + 4 Neutral) Krieger 0 do
      wennGespielt do
        prisma \x ->
          erhöhe Stärke selbst Dauerhaft (x * 1000)
      zahle (5 Neutral) do
        vision 1
  , mkAllmagie "Kolossale Stärke" (6 Neutral) do
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 8000
  , mkMagieDauerHaft "Kristallobelisk" (3 Neutral) $
      einmalProRunde $ wähle $ spende 1
  , mkWesen "Lurs Konstrukt" (5 Neutral) Konstrukt 1000 do
      wennGespielt $ ziehe 1
      blockierung
  , mkMagieDauerHaft "Magiestein der Arkanen Seele" (1 Neutral) do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        heile 1
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
      zahle (1 Neutral + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
  , mkMagieDauerHaft "Magiestein der Erdkraft" (1 Neutral) do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 3000
      zahle (3 Neutral + 2 Wald) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) Dauerhaft 3000
  , mkMagieDauerHaft "Magiestein der Erhebung" (1 Neutral) do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        zerstöre $ eine magie
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        verringereUndZerstöre (alle wesen) BisZumEndeDesZuges 3000
  , mkMagieDauerHaft "Magiestein der Erzürnung" (1 Neutral) do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
      zahle (2 Neutral + 1 Feuer) do
        opfere selbst
        vision 2
        zeigeObenVomDeck 2 LesbarKosten \x -> erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges (x * 1000)
      zahle (2 Neutral + 2 Wald) do
        opfere selbst
        bringeInsSpiel 1 schirmBestie
  , mkMagieDauerHaft "Magiestein der Feuerkraft" (1 Neutral) do
      zahle (1 Feuer) do
        opfere selbst
        gibFähigkeit (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges doppelzerstörung
      zahle (4 Feuer) do
        opfere selbst
        verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges 2000
        erhöhe Stärke (ein $ eigenes <> wesen) BisZumEndeDesZuges 2000
  , mkMagieDauerHaft "Magiestein der Finsterkraft" (1 Neutral) do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
      zahle (5 Neutral + 1 Tod) do
        opfere selbst
        einSpielerOpfertEinWesen
  , mkMagieDauerHaft "Magiestein der Lichtkraft" (1 Neutral) do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
      zahle (1 Neutral + 2 Licht) do
        opfere selbst
        heile 1
  , mkMagieDauerHaft "Magiestein der Manipulation" (1 Neutral) do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        siehHandkartenAnUndEntferneEineAusDemSpiel
      zahle (2 Neutral + 2 Wind) do
        opfere selbst
        nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
      zahle (4 Neutral + 2 Wasser) do
        opfere selbst
        ziehe 2
  , mkMagieDauerHaft "Magiestein der Säuberung" (1 Neutral) do
      zahle (2 Neutral + 1 Licht) do
        opfere selbst
        anzahlVon (alle $ eigene <> wesen <> aufDemFeld) heile
      zahle (2 Neutral + 1 Wald) do
        schaueObenVomDeck 5 do
          zeigeVorUndNimmtAufDieHand (ein wesen)
          legeRestUnterDeck
      zahle (4 Neutral + 1 Wasser) do
        opfere selbst
        anzahlVon (alle $ eigene <> wesen <> aufDemFeld) ziehe
  , mkMagieDauerHaft "Magiestein der Wasserkraft" (1 Neutral) do
      zahle (1 Wasser) do
        opfere selbst
        vision 3
      zahle (5 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 2
  , mkMagieDauerHaft "Magiestein der Windkraft" (1 Neutral) do
      zahle (2 Neutral + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      zahle (4 Neutral + 1 Wind) do
        opfere selbst
        ziehe 3
        wirfAb 2 SpendetNicht
  , mkMagieDauerHaft "Magiestein des Chaos" (1 Neutral) do
      zahle (1 Neutral + 1 Feuer) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges doppelzerstörung
      zahle (1 Neutral + 2 Wind) do
        opfere selbst
        vision 1
        schaueObenVomDeck 1 do
          wähleAktion
            [ zeigeVorUndNimmtAufDieHand $ eine karte
            , zeigeVorUndWirfAb $ eine karte
            ]
      zahle (4 Neutral + 2 Tod) do
        opfere selbst
        zerstöre $ ein wesen
  , mkMagieDauerHaft "Magiestein des Nexus" (1 Neutral) do
      zahle (1 Feuer) do
        opfere selbst
        gibFähigkeit (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges kannNichtAbwehren
      zahle (2 Neutral + 1 Wald) do
        opfere selbst
        legeVomDeckAufDenFriedhof 3 SpendetNicht
        nimmAufDieHand (eine aufDemFriedHof)
      zahle (2 Neutral + 2 Licht) do
        opfere selbst
        bringeInsSpielAusZiel (ein $ wesen <> aufDemFriedHof <> kostetMaximal 3)
  , mkMagieDauerHaft "Magiestein des Strahlens" (1 Neutral) do
      zahle (1 + 1 Feuer) do
        opfere selbst
        zerstöre (ein $ wesen <> kostetMaximal 3)
      zahle (1 + 2 Wind) do
        opfere selbst
        vision 1
        ziehe 1
      zahle (1 + 2 Licht) do
        opfere selbst
        verringereUndZerstöre (alle wesen) BisZumEndeDesZuges 3000
  , mkMagieDauerHaft "Magiestein für Belebung und Erhebung" (1 Neutral) do
      zahle (1 Licht) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges lebensentzug
      zahle (1 Feuer) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges doppelzerstörung
  , mkMagieDauerHaft "Magiestein für Einheit und Reinheit" (1 Neutral) do
      zahle (3 + 1 Wald) do
        opfere selbst
        anzahlVon (alle $ eigene <> wesen <> aufDemFeld) ziehe
      zahle (4 + 1 Licht) do
        opfere selbst
        bringeKopieInsSpiel (ein $ eigene <> wesen <> aufDemFeld)
  , mkMagieDauerHaft "Magiestein für Entdeckung und Vollstreckung" (1 Neutral) do
      zahle (1 + 1 Wind) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff WennNichtAbgewehrtWird $ ziehe 1
      zahle (1 + 3 Tod) do
        opfere selbst
        siehHandkartenAnUndEntferneEineAusDemSpiel
  , mkMagieDauerHaft "Magiestein für Gabe und Habe" (1 Neutral) do
      zahle (2 + 2 Licht) do
        opfere selbst
        heile 2
      zahle (3 + 2 Wind) do
        opfere selbst
        anzahlVon (alle $ eigene <> karten <> aufDerHand) \kartenAufDerHand ->
          anzahlSchicksalsmächte Du \schicksalsmächte ->
            ziehe $ schicksalsmächte - kartenAufDerHand
  , mkMagieDauerHaft "Magiestein für Hingabe und Eingabe" (1 Neutral) do
      zahle (1 + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ eigenes <> wesen <> aufDemFriedHof)
      zahle (1 + 1 Wasser) do
        opfere selbst
        nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
  , mkMagieDauerHaft "Magiestein für Krieg und Sieg" (1 Neutral) do
      zahle (2 + 2 Feuer) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges kriegsschrei
      zahle (2 + 2 Wald) do
        opfere selbst
        wähleZiel (ein $ eigenes <> wesen <> aufDemFeld) $
          wähleZiel (ein $ gegnerisches <> wesen <> aufDemFeld) . zerstöreSchwächeres
  , mkMagieDauerHaft "Magiestein für Mut und Flut" (1 Neutral) do
      zahle (2 + 1 Licht) do
        opfere selbst
        heile 1
      zahle (7 + 1 Wasser) do
        opfere selbst
        gibInsDeck Oben (bisZu 3 $ gegnerisches <> wesen <> aufDemFeld)
  , mkMagieDauerHaft "Magiestein für Neid und Leid" (1 Neutral) do
      zahle (1 + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      zahle (5 + 1 Feuer) do
        opfere selbst
        schade 1
  , mkMagieDauerHaft "Magiestein für Sicht und Verzicht" (1 Neutral) do
      zahle (1 + 1 Wasser) do
        opfere selbst
        vision 1
        ziehe 1
      zahle (2 + 1 Wind) do
        opfere selbst
        wähleZiel (ein $ wesen <> aufDemFeld) \ziel -> do
          verringere Stärke ziel Dauerhaft 2000
          gibFähigkeit ziel BisZumEndeDesZuges kannNichtAbwehren
  , mkMagieDauerHaft "Magiestein für Stärke und Werke" (1 Neutral) do
      zahle (1 Wasser) do
        opfere selbst
        schaueObenVomDeck 4 do
          zeigeVorUndNimmtAufDieHand (eine magie)
          legeRestUnterDeck
      zahle (1 Wald) do
        opfere selbst
        schaueObenVomDeck 4 do
          zeigeVorUndNimmtAufDieHand (ein wesen)
          legeRestUnterDeck
  , mkMagieDauerHaft "Magiestein für Stehlen und Seelen" (1 Neutral) do
      zahle (3 + 1 Tod) do
        opfere selbst
        gegnerOpfert (ein wesen)
      zahle (3 + 1 Licht) do
        opfere selbst
        bringeInsSpiel 3 faehrgeist
  , mkMagieDauerHaft "Magiestein für Streckung und Erweckung" (1 Neutral) do
      zahle (2 + 1 Wald) do
        opfere selbst
        schaueObenVomDeck 4 do
          zeigeVorUndNimmtAufDieHand $ bisZu 1 wesen
          legeRestAufDenFriedhof SpendetNicht
      zahle (4 + 2 Tod) do
        opfere selbst
        bringeInsSpielAusZiel (ein $ wesen <> aufDemFriedHof)
  , mkMagieDauerHaft "Magiestein für Tribut und Armut" (1 Neutral) do
      zahle (2 + 1 Tod) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff ZuBeginn $ gegnerWirfAb 1 SpendetNicht
      zahle (4 + 1 Feuer) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff ZuBeginn $ entferneAusDemSpiel (ein aufDemFriedHof)
  , mkMagieDauerHaft "Magiestein für Überwinden und Verschwinden" (1 Neutral) do
      zahle (1 Wald) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
      zahle (5 + 1 Wind) do
        opfere selbst
        gibInsDeck (AnPosition 2) (ein $ wesen <> aufDemFeld)
  , mkMagieDauerHaft "Magiestein für Zunder und Wunder" (1 Neutral) do
      zahle (1 + 1 Feuer) do
        opfere selbst
        zerstöre (ein $ wesen <> stärkeMaximal 4000)
      zahle (2 + 2 Wasser) do
        opfere selbst
        ziehe 2
  , mkMagieDauerHaft "Planumstein" (1 Neutral) do
      zahle (X Neutral) do
        opfere selbst
        -- TODO: add a test, X is the number of elements used to pay this effect
        prisma \x ->
          verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges (x * 1000)
  , mkWesen "Runensteinmagier" (2 Neutral) Magier 2000 do
      -- ermaechtigung 6 \x -> do
      --   erhöhe Stärke selbst Dauerhaft (x * 1000)
      --   ziehe x
      keinEffekt
  , mkAusrüstung "Stahlschwert" (2 Neutral) $
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
  , mkMagie "Steinbeseelung" (1 Neutral) do
      -- nimmAufDieHand (bisZu 2 $ hatTag Magiestein <> aufDemFriedHof)
      keinEffekt
  ]
