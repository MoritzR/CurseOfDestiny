{-# LANGUAGE ApplicativeDo #-}
module Cards where

import Trigger
import CardEffect
import Element


data CardType
  = Allmagie
  | Gegenmagie
  | Magie
  | MagieDauerhaft
  | Wesen Wesenstyp Stärke

data Wesenstyp
  = Konstrukt
  | Magier
  | Krieger

type Stärke = Int

-- In my TGC, how do I best model different card types? In my game I have two main types of cards. Creatures and Spells. Creatures have power, e.g. 4000, and
--   can battle other creatures, but not spells. Some cards can target any kind of card, like "destroy one card" or "prevent a card from activating", while others
--   will only target either creatures or spells.

data Card = Card
  { name :: String
  , cost :: Kosten
  , cardType :: CardType
  , trigger :: Trigger
  }

-- series 26
series26 = [
  Card {
    name = "Edors Konstruct",
    cardType = Wesen Konstrukt 1000,
    cost = 1 Neutral,
    trigger = zahle (5 Neutral) do
      erhöhe Stärke EinAnderesWesen Dauerhaft 1000
  },
  Card {
    name = "Energieladung",
    cardType = Allmagie,
    cost = 2 Neutral,
    trigger = wennGespielt do
      erhöhe Stärke EinAnderesWesen BisZumEndeDesZuges 2000
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
          erhöhe Stärke Selbst Dauerhaft (x * 1000)
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
        erhöhe Stärke EinAnderesWesen Dauerhaft 8000
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
        opfere Selbst
        heile 1
        pure ()
      zahle (1 Neutral + 1 Wasser) do
        opfere Selbst
        ziehe 1
        pure ()
      zahle (1 Neutral + 1 Wind) do
        opfere Selbst
        gibAufDieHandZurück EinAnderesWesen
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Erdkraft",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Wald) do
        opfere Selbst
        erhöhe Stärke EinAnderesWesen BisZumEndeDesZuges 3000
        pure ()
      zahle (3 Neutral + 2 Wald) do
        opfere Selbst
        erhöhe Stärke AlleWesenUnterDeinerKontrolle Dauerhaft 3000
        pure ()
      pure ()
  },
  Card {
    name = "Magiestein der Erhebung",
    cardType = MagieDauerhaft,
    cost = 1 Neutral,
    trigger = do
      zahle (1 Neutral + 1 Wald) do
        opfere Selbst
        zerstöre EinNichtWesen
        pure ()
      zahle (1 Neutral + 1 Wasser) do
        opfere Selbst
        ziehe 1
        pure ()
      zahle (2 Neutral + 1 Tod) do
        opfere Selbst
        verringereUndZerstöre AlleWesen BisZumEndeDesZuges 3000
        pure ()
      pure ()
  }
  ]

