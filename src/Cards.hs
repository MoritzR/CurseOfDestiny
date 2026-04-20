{-# LANGUAGE ApplicativeDo #-}
module Cards where

import Trigger
import CardEffect
import DataTypesNew
import Target

-- tokens
schirmBestie = Card {
      name = "Schirmbestie",
    cardType = Wesen Bestie 4000,
    cost = 2 Wald,
    trigger = pure ()
  }

-- series

series26 = [
  Card {
    name = "Edors Konstruct",
    cardType = Wesen Konstrukt 1000,
    cost = 1 Neutral,
    trigger = zahle (5 Neutral) do
      erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 1000
  },
  Card {
    name = "Energieladung",
    cardType = Allmagie,
    cost = 2 Neutral,
    trigger = wennGespielt do
      erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 2000
  },
  Card {
    name = "Fehrens Obelisk",
    cardType = MagieDauerhaft,
    cost = 3 Neutral,
    trigger = do
      wennGespielt do
        ziehe 1
      zahle (5 Neutral) do
        vision 1
      pure ()
  },
  Card {
    name = "Forscher der Royalen Akademie",
    cardType = Wesen Magier 2000,
    cost = 4 Neutral,
    trigger = do
      wennGespielt do
        vision 3
  },
  Card {
    name = "Hemtaras Krieger",
    cardType = Wesen Krieger 0,
    cost = X Neutral + 4 Neutral,
    trigger = do
      wennGespielt do
        prisma \x ->
          erhöhe Stärke selbst Dauerhaft (x * 1000)
      zahle (5 Neutral) do
        vision 1
      pure ()
  },
  Card {
    name = "Kolossale Stärke",
    cardType = Allmagie,
      cost = 6 Neutral,
    trigger = do
      wennGespielt do
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 8000
  },
  Card {
    name = "Kristallobelisk",
    cardType = MagieDauerhaft,
    cost = 3 Neutral,
    trigger = einmalProRunde $ wähle $ spende 1
  },
  Card {
    name = "Lurs Konstrukt",
    cardType = Wesen Konstrukt 1000,
    cost = 5 Neutral,
    trigger = do
      wennGespielt $ ziehe 1
      blockierung
      pure ()
  },
  Card {
    name = "Magiestein der Arkanen Seele",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        heile 1
        pure ()
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
        pure ()
      zahle (1 Neutral + 1 Wind) do
        opfere selbst
        gibAufDieHandZurück (ein $ wesen <> aufDemFeld)
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Erdkraft",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges 3000
        pure ()
      zahle (3 Neutral + 2 Wald) do
        opfere selbst
        erhöhe Stärke (alle $ eigene <> wesen) Dauerhaft 3000
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Erhebung",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Wald) do
        opfere selbst
        zerstöre (eine $ magie <> aufDemFeld)
        pure ()
      zahle (1 Neutral + 1 Wasser) do
        opfere selbst
        ziehe 1
        pure ()
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        verringereUndZerstöre (alle wesen) BisZumEndeDesZuges 3000
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Erzürnung",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
        pure ()
      zahle (2 Neutral + 1 Feuer) do
        opfere selbst
        vision 2
        zeigeObenVomDeck 2 LesbarKosten \x -> erhöhe Stärke (alle $ eigene <> wesen) BisZumEndeDesZuges (sum x * 1000)
        pure ()
      zahle (2 Neutral + 2 Wald) do
        opfere selbst
        beschwöre schirmBestie
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Feuerkraft",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Feuer) do
        opfere selbst
        gibFähigkeit (ein $ wesen <> aufDemFeld) BisZumEndeDesZuges doppelZerstörung
        pure ()
      zahle (4 Feuer) do
        opfere selbst
        verringere Stärke (ein $ gegnerisches <> wesen) BisZumEndeDesZuges 2000
        erhöhe Stärke (ein $ eigenes <> wesen) BisZumEndeDesZuges 2000
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Finsterkraft",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof)
        pure ()
      zahle (5 Neutral + 1 Tod) do
        opfere selbst
        einSpielerOpfertEinWesen
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Lichtkraft",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Licht) do
        opfere selbst
        erhöhe Stärke (ein $ wesen <> aufDemFeld) Dauerhaft 2000
        pure ()
      zahle (1 Neutral + 2 Licht) do
        opfere selbst
        heile 1
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Manipulation",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (2 Neutral + 1 Tod) do
        opfere selbst
        siehHandkartenAnUndEntferneEineAusDemSpiel 
        pure ()
      zahle (2 Neutral + 2 Wind) do
        opfere selbst
        nimmAufDieHand $ eine $ eigene <> (magie `oder` gegenmagie) <> aufDemFriedHof
        pure ()
      zahle (4 Neutral + 2 Wasser) do
        opfere selbst
        ziehe 2
        pure ()
      pure ()
  }
  ]
