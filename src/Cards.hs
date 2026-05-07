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

faehrgeist =
  Card
    { name = "Fährgeist"
    , cardType = Wesen Geist 1000
    , cost = 1 Tod + 1 Licht
    , trigger = lebensentzug
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
            bringeInsSpiel 1 schirmBestie
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
  , Card
      { name = "Magiestein für Hingabe und Eingabe"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 + 1 Tod) do
            opfere selbst
            nimmAufDieHand (ein $ eigenes <> wesen <> aufDemFriedHof)
          zahle (1 + 1 Wasser) do
            opfere selbst
            nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
      }
  , Card
      { name = "Magiestein für Krieg und Sieg"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 + 2 Feuer) do
            opfere selbst
            erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges 2000
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges kriegsschrei
          zahle (2 + 2 Wald) do
            opfere selbst
            wähleZiel (ein $ eigenes <> wesen <> aufDemFeld) $
              wähleZiel (ein $ gegnerisches <> wesen <> aufDemFeld) . zerstöreSchwächeres
      }
  , Card
      { name = "Magiestein für Mut und Flut"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 + 1 Licht) do
            opfere selbst
            heile 1
          zahle (7 + 1 Wasser) do
            opfere selbst
            gibInsDeck Oben (bisZu 3 $ gegnerisches <> wesen <> aufDemFeld)
      }
  , Card
      { name = "Magiestein für Neid und Leid"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 + 1 Wind) do
            opfere selbst
            gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
          zahle (5 + 1 Feuer) do
            opfere selbst
            schade 1
      }
  , Card
      { name = "Magiestein für Sicht und Verzicht"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 + 1 Wasser) do
            opfere selbst
            vision 1
            ziehe 1
          zahle (2 + 1 Wind) do
            opfere selbst
            wähleZiel (ein $ wesen <> aufDemFeld) \ziel -> do
              verringere Stärke ziel Dauerhaft 2000
              gibFähigkeit ziel BisZumEndeDesZuges kannNichtAbwehren
      }
  , Card
      { name = "Magiestein für Stärke und Werke"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
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
      }
  , Card
      { name = "Magiestein für Stehlen und Seelen"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (3 + 1 Tod) do
            opfere selbst
            gegnerOpfert (ein wesen)
          zahle (3 + 1 Licht) do
            opfere selbst
            bringeInsSpiel 3 faehrgeist
      }
  , Card
      { name = "Magiestein für Streckung und Erweckung"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 + 1 Wald) do
            opfere selbst
            schaueObenVomDeck 4 do
              zeigeVorUndNimmtAufDieHand $ bisZu 1 wesen
              legeRestAufDenFriedhof SpendetNicht
          zahle (4 + 2 Tod) do
            opfere selbst
            bringeInsSpielAusZiel (ein $ wesen <> aufDemFriedHof)
      }
  , Card
      { name = "Magiestein für Tribut und Armut"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (2 + 1 Tod) do
            opfere selbst
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
              beimAngriff ZuBeginn $ gegnerWirfAb 1 SpendetNicht
          zahle (4 + 1 Feuer) do
            opfere selbst
            gibFähigkeit (alle $ eigene <> wesen) BisZumEndeDesZuges do
              beimAngriff ZuBeginn $ entferneAusDemSpiel (ein aufDemFriedHof)
      }
  , Card
      { name = "Magiestein für Überwinden und Verschwinden"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 Wald) do
            opfere selbst
            erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
          zahle (5 + 1 Wind) do
            opfere selbst
            gibInsDeck (AnPosition 2) (ein $ wesen <> aufDemFeld)
      }
  , Card
      { name = "Magiestein für Zunder und Wunder"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (1 + 1 Feuer) do
            opfere selbst
            zerstöre (ein $ wesen <> stärkeMaximal 4000)
          zahle (2 + 2 Wasser) do
            opfere selbst
            ziehe 2
      }
  , Card
      { name = "Planumstein"
      , cardType = MagieDauerhaft
      , cost = 1 Neutral
      , trigger = do
          zahle (X Neutral) do
            opfere selbst
            -- TODO: add a test, X is the number of elements used to pay this effect
            prisma \x ->
              verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges (x * 1000)
      }
  , Card
      { name = "Runensteinmagier"
      , cardType = Wesen Magier 2000
      , cost = 2 Neutral
      , trigger = do
          -- ermaechtigung 6 \x -> do
          --   erhöhe Stärke selbst Dauerhaft (x * 1000)
          --   ziehe x
          keinEffekt
      }
  , Card
      { name = "Stahlschwert"
      , cardType = Ausrüstung
      , cost = 2 Neutral
      , trigger = wennGespielt do
          erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
      }
  , Card
      { name = "Steinbeseelung"
      , cardType = Magie
      , cost = 1 Neutral
      , trigger = do
          -- nimmAufDieHand (bisZu 2 $ hatTag Magiestein <> aufDemFriedHof)
          keinEffekt
      }
  ]
