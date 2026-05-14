{-# LANGUAGE RecordWildCards #-}

module Cards where

import CardEffect
import Data.Function ((&))
import DataTypes
import Target
import Trigger

mkWesen :: String -> Kosten -> Tag -> Int -> Trigger -> Card
mkWesen name cost wesenstyp stärke trigger =
  Card
    { name
    , cardType = Wesen stärke
    , cost
    , trigger
    , tags = []
    }
    & tag wesenstyp

tag :: Tag -> Card -> Card
tag theTag card = card{tags = card.tags <> [theTag]}

mk :: String -> CardType -> Kosten -> Trigger -> Card
mk name cardType cost trigger = Card{tags = [], ..}

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
  , mk "Energieladung" Allmagie (2 Neutral) $
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 2000
  , mk "Fehrens Obelisk" MagieDauerhaft (3 Neutral) do
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
  , mk "Kolossale Stärke" Allmagie (6 Neutral) do
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 8000
  , mk "Kristallobelisk" MagieDauerhaft (3 Neutral) $
      einmalProRunde $
        wähle $
          spende 1
  , mkWesen "Lurs Konstrukt" (5 Neutral) Konstrukt 1000 do
      wennGespielt $ ziehe 1
      blockierung
  , mk "Magiestein der Arkanen Seele" MagieDauerhaft (1 Neutral) do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        heile 1
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
      zahle (1 Neutral + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      & tag Magiestein
  , mk "Magiestein der Erdkraft" MagieDauerhaft (1 Neutral) do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 3000
      zahle (3 Neutral + 2 Wald) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) Dauerhaft 3000
      & tag Magiestein
  , mk "Magiestein der Erhebung" MagieDauerhaft (1 Neutral) do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        zerstöre $ eine magie
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        verringereUndZerstöre (alle wesen) BisZumEndeDesZuges 3000
      & tag Magiestein
  , mk "Magiestein der Erzürnung" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein der Feuerkraft" MagieDauerhaft (1 Neutral) do
      zahle (1 Feuer) do
        opfere selbst
        gibFähigkeit (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges doppelzerstörung
      zahle (4 Feuer) do
        opfere selbst
        verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges 2000
        erhöhe Stärke (ein $ eigenes <> wesen) BisZumEndeDesZuges 2000
      & tag Magiestein
  , mk "Magiestein der Finsterkraft" MagieDauerhaft (1 Neutral) do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
      zahle (5 Neutral + 1 Tod) do
        opfere selbst
        einSpielerOpfertEinWesen
      & tag Magiestein
  , mk "Magiestein der Lichtkraft" MagieDauerhaft (1 Neutral) do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
      zahle (1 Neutral + 2 Licht) do
        opfere selbst
        heile 1
      & tag Magiestein
  , mk "Magiestein der Manipulation" MagieDauerhaft (1 Neutral) do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        siehHandkartenAnUndEntferneEineAusDemSpiel
      zahle (2 Neutral + 2 Wind) do
        opfere selbst
        nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
      zahle (4 Neutral + 2 Wasser) do
        opfere selbst
        ziehe 2
      & tag Magiestein
  , mk "Magiestein der Säuberung" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein der Wasserkraft" MagieDauerhaft (1 Neutral) do
      zahle (1 Wasser) do
        opfere selbst
        vision 3
      zahle (5 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 2
      & tag Magiestein
  , mk "Magiestein der Windkraft" MagieDauerhaft (1 Neutral) do
      zahle (2 Neutral + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      zahle (4 Neutral + 1 Wind) do
        opfere selbst
        ziehe 3
        wirfAb 2 SpendetNicht
      & tag Magiestein
  , mk "Magiestein des Chaos" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein des Nexus" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein des Strahlens" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein für Belebung und Erhebung" MagieDauerhaft (1 Neutral) do
      zahle (1 Licht) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges lebensentzug
      zahle (1 Feuer) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges doppelzerstörung
      & tag Magiestein
  , mk "Magiestein für Einheit und Reinheit" MagieDauerhaft (1 Neutral) do
      zahle (3 + 1 Wald) do
        opfere selbst
        anzahlVon (alle $ eigene <> wesen <> aufDemFeld) ziehe
      zahle (4 + 1 Licht) do
        opfere selbst
        bringeKopieInsSpiel (ein $ eigene <> wesen <> aufDemFeld)
      & tag Magiestein
  , mk "Magiestein für Entdeckung und Vollstreckung" MagieDauerhaft (1 Neutral) do
      zahle (1 + 1 Wind) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff WennNichtAbgewehrtWird $ ziehe 1
      zahle (1 + 3 Tod) do
        opfere selbst
        siehHandkartenAnUndEntferneEineAusDemSpiel
      & tag Magiestein
  , mk "Magiestein für Gabe und Habe" MagieDauerhaft (1 Neutral) do
      zahle (2 + 2 Licht) do
        opfere selbst
        heile 2
      zahle (3 + 2 Wind) do
        opfere selbst
        anzahlVon (alle $ eigene <> karten <> aufDerHand) \kartenAufDerHand ->
          anzahlSchicksalsmächte Du \schicksalsmächte ->
            ziehe $ schicksalsmächte - kartenAufDerHand
      & tag Magiestein
  , mk "Magiestein für Hingabe und Eingabe" MagieDauerhaft (1 Neutral) do
      zahle (1 + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ eigenes <> wesen <> aufDemFriedHof)
      zahle (1 + 1 Wasser) do
        opfere selbst
        nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
      & tag Magiestein
  , mk "Magiestein für Krieg und Sieg" MagieDauerhaft (1 Neutral) do
      zahle (2 + 2 Feuer) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges kriegsschrei
      zahle (2 + 2 Wald) do
        opfere selbst
        wähleZiel (ein $ eigenes <> wesen <> aufDemFeld) $
          wähleZiel (ein $ gegnerisches <> wesen <> aufDemFeld) . zerstöreSchwächeres
      & tag Magiestein
  , mk "Magiestein für Mut und Flut" MagieDauerhaft (1 Neutral) do
      zahle (2 + 1 Licht) do
        opfere selbst
        heile 1
      zahle (7 + 1 Wasser) do
        opfere selbst
        gibInsDeck Oben (bisZu 3 $ gegnerisches <> wesen <> aufDemFeld)
      & tag Magiestein
  , mk "Magiestein für Neid und Leid" MagieDauerhaft (1 Neutral) do
      zahle (1 + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      zahle (5 + 1 Feuer) do
        opfere selbst
        schade 1
      & tag Magiestein
  , mk "Magiestein für Sicht und Verzicht" MagieDauerhaft (1 Neutral) do
      zahle (1 + 1 Wasser) do
        opfere selbst
        vision 1
        ziehe 1
      zahle (2 + 1 Wind) do
        opfere selbst
        wähleZiel (ein $ wesen <> aufDemFeld) \ziel -> do
          verringere Stärke ziel Dauerhaft 2000
          gibFähigkeit ziel BisZumEndeDesZuges kannNichtAbwehren
      & tag Magiestein
  , mk "Magiestein für Stärke und Werke" MagieDauerhaft (1 Neutral) do
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
      & tag Magiestein
  , mk "Magiestein für Stehlen und Seelen" MagieDauerhaft (1 Neutral) do
      zahle (3 + 1 Tod) do
        opfere selbst
        gegnerOpfert (ein wesen)
      zahle (3 + 1 Licht) do
        opfere selbst
        bringeInsSpiel 3 faehrgeist
      & tag Magiestein
  , mk "Magiestein für Streckung und Erweckung" MagieDauerhaft (1 Neutral) do
      zahle (2 + 1 Wald) do
        opfere selbst
        schaueObenVomDeck 4 do
          zeigeVorUndNimmtAufDieHand $ bisZu 1 wesen
          legeRestAufDenFriedhof SpendetNicht
      zahle (4 + 2 Tod) do
        opfere selbst
        bringeInsSpielAusZiel (ein $ wesen <> aufDemFriedHof)
      & tag Magiestein
  , mk "Magiestein für Tribut und Armut" MagieDauerhaft (1 Neutral) do
      zahle (2 + 1 Tod) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff ZuBeginn $ gegnerWirfAb 1 SpendetNicht
      zahle (4 + 1 Feuer) do
        opfere selbst
        gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
          beimAngriff ZuBeginn $ entferneAusDemSpiel (ein aufDemFriedHof)
      & tag Magiestein
  , mk "Magiestein für Überwinden und Verschwinden" MagieDauerhaft (1 Neutral) do
      zahle (1 Wald) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
      zahle (5 + 1 Wind) do
        opfere selbst
        gibInsDeck (AnPosition 2) (ein $ wesen <> aufDemFeld)
      & tag Magiestein
  , mk "Magiestein für Zunder und Wunder" MagieDauerhaft (1 Neutral) do
      zahle (1 + 1 Feuer) do
        opfere selbst
        zerstöre (ein $ wesen <> stärkeMaximal 4000)
      zahle (2 + 2 Wasser) do
        opfere selbst
        ziehe 2
      & tag Magiestein
  , mk "Planumstein" MagieDauerhaft (1 Neutral) do
      zahle (X Neutral) do
        opfere selbst
        -- TODO: add a test, X is the number of elements used to pay this effect
        prisma \x ->
          verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges (x * 1000)
  , mkWesen "Runensteinmagier" (2 Neutral) Magier 2000 do
      ermächtigung 6 \x -> do
        erhöhe Stärke selbst Dauerhaft (x * 1000)
        ziehe x
  , mk "Stahlschwert" Ausrüstung (2 Neutral) $
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
  , mk "Steinbeseelung" Magie (1 Neutral) do
      wennGespielt do
        nimmAufDieHand (bisZu 2 $ hatTag Magiestein <> aufDemFriedHof)
  , mk "Zaubernetzwerk" Magie (12 Neutral) do
      -- TODO: support static cost reduction, e.g. `kostenReduktion (fürJeden $ hatTag Magiestein <> aufDemFriedHof) 1`
      wennGespielt do
        bringeInsSpielAusZiel (alle $ hatTag Magiestein <> aufDemFriedHof)
  , mkWesen "Abenteurer von Alzoth" (2 + 1 Licht) Krieger 3000 do
      wirdZielVon (einer $ anderen <> eigenen <> karte) do
        erhöhe Stärke selbst Dauerhaft 1000
  , mkWesen "Ahnenbeschwörer" (2 + 3 Licht) Magier 4000 do
      wennGespielt do
        bringeInsSpiel 1 faehrgeist
  , mkWesen "Anrufer der Arkanen Macht" (1 + 1 Licht) Magier 2000 do
      blockierung
      zahle 12 do
        erhöhe Stärke (alle $ eigene <> wesen) Dauerhaft 3000
  , mk "Antike Klinge" Ausrüstung (1 + 2 Licht) do
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
        bringeInsSpiel 1 faehrgeist
  , mkWesen "Archon des gleißenden Lichts" (3 + 2 Licht) Archon 5500 do
      amBeginnDerKampfPhase do
        wähleZiel (ein $ wesen <> aufDemFeld) \ziel -> do
          erhöhe Stärke ziel BisZumEndeDesZuges 3000
          -- wähleAus [doppelzerstörung, lebensentzug, doppelangriff] $
            -- gibFähigkeit ziel BisZumEndeDesZuges
  , mkWesen "Atos-Hellebarier" (1 Licht) Krieger 1000 do
      keinEffekt
      -- wenn (eigenes <> wesen) insSpielKommt do
      --   erhöhe Stärke selbst Dauerhaft 1000
  , mkWesen "Azokur-Brigarde" (1 + 1 Licht) Krieger 2000 do
      wirdZielVon (einer $ anderen <> eigenen <> karte) do
        erhöhe Stärke selbst Dauerhaft 1000
        ziehe 1
        heile 1
  , mk "Beherrschung der Mächte" Magie (3 + 1 Licht) do
      keinEffekt
      wennGespielt do
        prisma \x ->
        -- TODO: check if we can remove `aufDemFeld` restriction from `erhöhe` and imply it instead
          erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft (x * 1000)
  ]
