{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target where

import DataTypesNew (Card (..), CardType (..), EinZiel (..), Ziel (..), ZielAnzahl (..))
import Element (gesamtKosten)

oder :: EinZiel -> EinZiel -> EinZiel
oder a b = EinZiel (a.description <> " oder " <> b.description) $ \card -> a.filter card || b.filter card

karte = EinZiel "Karte" (const True)
wesen = EinZiel "Wesen" \card -> case card.cardType of
  Wesen _ _ -> True
  _ -> False
magie = EinZiel "Magie" \card -> case card.cardType of
  Magie -> True
  _ -> False
gegenmagie = EinZiel "Gegenmagie" \card -> case card.cardType of
  Gegenmagie -> True
  _ -> False
aufDemFeld = EinZiel "auf dem Feld" (const undefined)
aufDemFriedHof = EinZiel "auf dem Friedhof" (const undefined)
eigene = EinZiel "eigene" (const undefined)
eigenes = eigene{description = "eigenes"}
gegnerisches = EinZiel "gegnerisches" (const undefined)
kostetMaximal anzahl = EinZiel ("mit kosten von " <> show anzahl <> " oder weniger") \card -> gesamtKosten card.cost <= anzahl

selbst = Ziel{anzahl = Undefiniert, ziel = EinZiel "diese Karte" (const undefined)}

ein = Ziel Ein
eine = Ziel Eine
alle = Ziel Alle
