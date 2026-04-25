module Cards where

import CardEffect
import DataTypes
import Target
import Trigger

-- tokens
schirmBestie =
  Card
    { name = "Schirmbestie"
    , cardType = Wesen Bestie 4000
    , cost = 2 Wald
    , trigger = keinEffekt
    }

-- series
series26 =
  [ Card
      { name = "Edors Konstruct"
      , cardType = Wesen Konstrukt 1000
      , cost = 1 Neutral
      , trigger = zahle (5 Neutral) do
          erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 1000
      }
  , Card
      { name = "Energieladung"
      , cardType = Allmagie
      , cost = 2 Neutral
      , trigger = wennGespielt do
          erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 2000
      }
  , Card
      { name = "Fehrens Obelisk"
      , cardType = MagieDauerhaft
      , cost = 3 Neutral
      , trigger = do
          wennGespielt do
            ziehe 1
          zahle (5 Neutral) do
            vision 1
      }
  , Card
      { name = "Forscher der Royalen Akademie"
      , cardType = Wesen Magier 2000
      , cost = 4 Neutral
      , trigger = do
          wennGespielt do
            vision 3
      }
  , Card
      { name = "Hemtaras Krieger"
      , cardType = Wesen Krieger 0
      , cost = X Neutral + 4 Neutral
      , trigger = do
          wennGespielt do
            prisma \x ->
              erhöhe Stärke selbst Dauerhaft (x * 1000)
          zahle (5 Neutral) do
            vision 1
      }
  , Card
      { name = "Kolossale Stärke"
      , cardType = Allmagie
      , cost = 6 Neutral
      , trigger = do
          wennGespielt do
            erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 8000
      }
  , Card
      { name = "Kristallobelisk"
      , cardType = MagieDauerhaft
      , cost = 3 Neutral
      , trigger = einmalProRunde $ wähle $ spende 1
      }
  , Card
      { name = "Lurs Konstrukt"
      , cardType = Wesen Konstrukt 1000
      , cost = 5 Neutral
      , trigger = do
          wennGespielt $ ziehe 1
          blockierung
      }
  , Card
      { name = "Magiestein der Arkanen Seele"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Neutral + 1 Licht) do
            opfere selbst
            heile 1
          zahle (1 Neutral + 1 Wasser) do
            opfere selbst
            ziehe 1
          zahle (1 Neutral + 1 Wind) do
            opfere selbst
            gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
      }
  , Card
      { name = "Magiestein der Erdkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Neutral + 1 Wald) do
            opfere selbst
            erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 3000
          zahle (3 Neutral + 2 Wald) do
            opfere selbst
            erhöhe Stärke (alle $ eigene <> wesen) Dauerhaft 3000
      }
  , Card
      { name = "Magiestein der Erhebung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Neutral + 1 Wald) do
            opfere selbst
            zerstöre $ eine magie
          zahle (1 Neutral + 1 Wasser) do
            opfere selbst
            ziehe 1
          zahle (2 Neutral + 1 Tod) do
            opfere selbst
            verringereUndZerstöre (alle wesen) BisZumEndeDesZuges 3000
      }
  , Card
      { name = "Magiestein der Erzürnung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 Neutral + 1 Tod) do
            opfere selbst
            nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
          zahle (2 Neutral + 1 Feuer) do
            opfere selbst
            vision 2
            zeigeObenVomDeck 2 LesbarKosten \x -> erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges (x * 1000)
          zahle (2 Neutral + 2 Wald) do
            opfere selbst
            bringeInsSpiel schirmBestie
      }
  , Card
      { name = "Magiestein der Feuerkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Feuer) do
            opfere selbst
            gibFähigkeit (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges doppelzerstörung
          zahle (4 Feuer) do
            opfere selbst
            verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges 2000
            erhöhe Stärke (ein $ eigenes <> wesen) BisZumEndeDesZuges 2000
      }
  , Card
      { name = "Magiestein der Finsterkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 Neutral + 1 Tod) do
            opfere selbst
            nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
          zahle (5 Neutral + 1 Tod) do
            opfere selbst
            einSpielerOpfertEinWesen
      }
  , Card
      { name = "Magiestein der Lichtkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Neutral + 1 Licht) do
            opfere selbst
            erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
          zahle (1 Neutral + 2 Licht) do
            opfere selbst
            heile 1
      }
  , Card
      { name = "Magiestein der Manipulation"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 Neutral + 1 Tod) do
            opfere selbst
            siehHandkartenAnUndEntferneEineAusDemSpiel
          zahle (2 Neutral + 2 Wind) do
            opfere selbst
            nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
          zahle (4 Neutral + 2 Wasser) do
            opfere selbst
            ziehe 2
      }
  , Card
      { name = "Magiestein der Säuberung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
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
      }
  , Card
      { name = "Magiestein der Wasserkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Wasser) do
            opfere selbst
            vision 3
          zahle (5 Neutral + 1 Wasser) do
            opfere selbst
            ziehe 2
      }
  , Card
      { name = "Magiestein der Windkraft"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 Neutral + 1 Wind) do
            opfere selbst
            gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
          zahle (4 Neutral + 1 Wind) do
            opfere selbst
            ziehe 3
            wirfAb 2 SpendetNicht
      }
  , Card
      { name = "Magiestein des Chaos"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
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
      }
  , Card
      { name = "Magiestein des Nexus"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
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
      }
  , Card
      { name = "Magiestein des Strahlens"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
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
      }
  , Card
      { name = "Magiestein für Belebung und Erhebung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Licht) do
            opfere selbst
            erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges lebensentzug
          zahle (1 Feuer) do
            opfere selbst
            erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges doppelzerstörung
      }
  , Card
      { name = "Magiestein für Einheit und Reinheit"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (3 + 1 Wald) do
            opfere selbst
            anzahlVon (alle $ eigene <> wesen <> aufDemFeld) ziehe
          zahle (4 + 1 Licht) do
            opfere selbst
            bringeKopieInsSpiel (ein $ eigene <> wesen <> aufDemFeld)
      }
  , Card
      { name = "Magiestein für Entdeckung und Vollstreckung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 + 1 Wind) do
            opfere selbst
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
              beimAngriff WennNichtAbgewehrtWird $ ziehe 1
          zahle (1 + 3 Tod) do
            opfere selbst
            siehHandkartenAnUndEntferneEineAusDemSpiel
      }
  , Card
      { name = "Magiestein für Gabe und Habe"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 + 2 Licht) do
            opfere selbst
            heile 2
          zahle (3 + 2 Wind) do
            opfere selbst
            anzahlVon (alle $ eigene <> karten <> aufDerHand) \kartenAufDerHand ->
              anzahlSchicksalsmächte Du \schicksalsmächte ->
                ziehe $ schicksalsmächte - kartenAufDerHand
      }
  ]
