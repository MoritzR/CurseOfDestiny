{-# LANGUAGE OverloadedStrings #-}

module Web.Snapshot (
  snapshotGameState,
) where

import Data.Text (Text, pack)
import DataTypes
import Element (gesamtKosten)
import Interpreter.Describe (describeCard, describeGrantedTrigger)
import Interpreter.Game (creatureStrength)
import Web.Protocol

snapshotGameState :: GameState -> GameSnapshot
snapshotGameState state =
  GameSnapshot
    { currentPlayer = renderPlayerId state.currentPlayer
    , nextCardId = state.nextCardId
    , players = [snapshotPlayer player1, snapshotPlayer player2]
    }
 where
  (player1, player2) = state.players

snapshotPlayer :: Player -> PlayerSnapshot
snapshotPlayer player =
  PlayerSnapshot
    { playerId = renderPlayerId player.playerId
    , name = pack player.name
    , schicksalsmacht = player.schicksalsmacht
    , hand = snapshotCard <$> player.hand
    , field = snapshotCard <$> player.field
    , graveyard = snapshotCard <$> player.graveyard
    , deck = snapshotCard <$> player.deck
    }

snapshotCard :: CardInPlay -> CardSnapshot
snapshotCard cardInPlay =
  CardSnapshot
    { cardId = cardInPlay.id.get
    , owner = renderPlayerId cardInPlay.owner
    , name = pack cardInPlay.card.name
    , cost = pack $ renderCost cardInPlay.card.cost
    , cardType = pack $ renderCardType cardInPlay.card.cardType
    , baseStrength = case cardInPlay.card.cardType of
        Wesen strength -> Just strength
        _ -> Nothing
    , currentStrength = case cardInPlay.card.cardType of
        Wesen _ -> Just $ creatureStrength cardInPlay
        _ -> Nothing
    , tags = pack . show <$> cardInPlay.card.tags
    , description = pack $ describeCard cardInPlay.card
    , modifications = snapshotModification <$> cardInPlay.modifications
    }

snapshotModification :: Modification -> ModificationSnapshot
snapshotModification = \case
  StärkeModifikation dauer delta ->
    ModificationSnapshot
      { kind = "strength"
      , duration = pack $ show dauer
      , amount = Just delta
      , description = pack $ show delta
      }
  FähigkeitsModifikation dauer trigger ->
    ModificationSnapshot
      { kind = "ability"
      , duration = pack $ show dauer
      , amount = Nothing
      , description = pack $ describeGrantedTrigger trigger
      }

renderPlayerId :: PlayerId -> Text
renderPlayerId = \case
  Player1 -> "player1"
  Player2 -> "player2"

renderCardType :: CardType -> String
renderCardType = \case
  Allmagie -> "allmagie"
  Gegenmagie -> "gegenmagie"
  Magie -> "magie"
  Ausrüstung -> "ausruestung"
  MagieDauerhaft -> "dauerhaft"
  Wesen _ -> "wesen"

renderCost :: Kosten -> String
renderCost cardCost = show $ gesamtKosten cardCost
