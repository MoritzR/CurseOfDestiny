{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import DataTypes
import Element (gesamtKosten)
import Optics (filtered, traversed, (%), (^..))
import Optics.Traversal (both)

oder :: EinZiel -> EinZiel -> EinZiel
oder a b =
  EinZiel (a.description <> " oder " <> b.description) $
    \state sourceId -> a.candidates state sourceId <> b.candidates state sourceId

karte :: EinZiel
karte = EinZiel "Karte" \state _ -> state ^.. allCards

karten :: EinZiel
karten = karte{description = "Karten"}

wesen :: EinZiel
wesen = EinZiel "Wesen" \state _ ->
  (state ^.. allCards)
    & filter \cardInPlay -> case cardInPlay.card.cardType of
      Wesen _ _ -> True
      _ -> False

magie :: EinZiel
magie = EinZiel "Magie" \state _ ->
  (state ^.. allCards)
    & filter \cardInPlay -> case cardInPlay.card.cardType of
      Allmagie; Gegenmagie; Magie; MagieDauerhaft -> True
      _ -> False

gegenmagie :: EinZiel
gegenmagie = EinZiel "Gegenmagie" \state _ ->
  (state ^.. allCards)
    & filter \cardInPlay -> case cardInPlay.card.cardType of
      Gegenmagie -> True
      _ -> False

aufDemFeld :: EinZiel
aufDemFeld = EinZiel "auf dem Feld" \state _ -> state ^.. #players % both % #field % traversed

aufDemFriedHof :: EinZiel
aufDemFriedHof = EinZiel "auf dem Friedhof" \state _ -> state ^.. #players % both % #graveyard % traversed

aufDerHand :: EinZiel
aufDerHand = EinZiel "auf der Hand" \state _ -> state ^.. #players % both % #hand % traversed

eigene :: EinZiel
eigene = EinZiel "eigene" \state sourceId ->
  state ^.. allCards % filtered \cardInPlay ->
    zoneOwnerOf state cardInPlay.id == Just (ownerOfTriggeringCard state sourceId)

eigenes :: EinZiel
eigenes = eigene{description = "eigenes"}

gegnerisches :: EinZiel
gegnerisches = EinZiel "gegnerisches" \state sourceId ->
  state ^.. allCards % filtered \cardInPlay ->
    zoneOwnerOf state cardInPlay.id == Just (otherPlayer $ ownerOfTriggeringCard state sourceId)

kostetMaximal :: Int -> EinZiel
kostetMaximal maxKosten =
  EinZiel ("mit kosten von " <> show maxKosten <> " oder weniger") \state _ ->
    state ^.. allCards % filtered \cardInPlay ->
      gesamtKosten cardInPlay.card.cost <= maxKosten

selbst :: Ziel
selbst =
  Ziel
    { anzahl = Undefiniert
    , ziel = EinZiel "diese Karte" \state sourceId -> state ^.. allCards % filtered \cardInPlay -> cardInPlay.id == sourceId
    }

ein :: EinZiel -> Ziel
ein = Ziel Ein

eine :: EinZiel -> Ziel
eine = Ziel Eine

alle :: EinZiel -> Ziel
alle = Ziel Alle

bisZu :: Anzahl -> EinZiel -> Ziel
bisZu anzahl = Ziel (BisZu anzahl)

ownerOfTriggeringCard :: GameState -> CardId -> PlayerId
ownerOfTriggeringCard gameState sourceId =
  fromMaybe gameState.currentPlayer $
    zoneOwnerOf gameState sourceId <|> cardOwnerOf gameState sourceId

zoneOwnerOf :: GameState -> CardId -> Maybe PlayerId
zoneOwnerOf gameState cardId =
  firstMatchingOwner (.field) <|> firstMatchingOwner (.hand) <|> firstMatchingOwner (.graveyard) <|> firstMatchingOwner (.deck)
 where
  firstMatchingOwner zone =
    let (player1, player2) = gameState.players
     in if any ((== cardId) . (.id)) (zone player1)
          then Just player1.playerId
          else
            if any ((== cardId) . (.id)) (zone player2)
              then Just player2.playerId
              else Nothing

cardOwnerOf :: GameState -> CardId -> Maybe PlayerId
cardOwnerOf state cardId = (.owner) <$> firstMatchingCard
 where
  firstMatchingCard = findCard cardId (state ^.. allCards)

inZone :: (Player -> [CardInPlay]) -> GameState -> CardId -> Bool
inZone zone gameState cardId =
  let (player1, player2) = gameState.players
   in any ((== cardId) . (.id)) (zone player1) || any ((== cardId) . (.id)) (zone player2)

findCard :: CardId -> [CardInPlay] -> Maybe CardInPlay
findCard _ [] = Nothing
findCard cardId (cardInPlay : rest)
  | cardInPlay.id == cardId = Just cardInPlay
  | otherwise = findCard cardId rest

otherPlayer :: PlayerId -> PlayerId
otherPlayer = \case
  Player1 -> Player2
  Player2 -> Player1
