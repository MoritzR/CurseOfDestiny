{-# LANGUAGE OverloadedRecordDot #-}

module GameSpec where

import Cards (series26)
import Data.List (find)
import DataTypes
import Game
import GameActionParser (GameAction (..))
import GameState (initialGameState)
import Test.Hspec
import GameEffects ( ignoreLog, runChoiceInputConst )
import Effectful.State.Static.Local (execState)
import Effectful (runEff)
import Data.Function ((&))

spec :: Spec
spec = do
  describe "Game state transitions" $ do
    it "starts with an opening hand drawn from the deck" do
      let (player1, player2) = (drawOpeningHands initialGameState).players
      length player1.hand `shouldBe` 5
      length player2.hand `shouldBe` 5
      length player1.deck `shouldBe` length series26 - 5
      length player2.deck `shouldBe` length series26 - 5

    it "buffs a played creature when a spell is played afterwards" do
      state <- runGameActions 1 (drawOpeningHands initialGameState) [PlayFromHand 0, PlayFromHand 0]
      let ownField = cardsForPlayer Player1 state
          (player1, _) = state.players
      length ownField `shouldBe` 1
      fmap (.card.name) player1.graveyard `shouldBe` ["Energieladung"]
      fmap (.card.name) ownField `shouldBe` ["Edors Konstruct"]
      case ownField of
        [cardInPlay] -> creatureStrength cardInPlay `shouldBe` 3000
        _ -> expectationFailure "expected exactly one card on the field"

    it "can activate a permanent card effect that sacrifices itself and buffs a creature" do
      let state = withPlayer1Hand ["Edors Konstruct", "Magiestein der Erdkraft"] (drawOpeningHands initialGameState)
      finalState <- runGameActions 1 state [PlayFromHand 0, PlayFromHand 0, ActivateFromField 1]
      let ownField = cardsForPlayer Player1 finalState
          (player1, _) = finalState.players
      length ownField `shouldBe` 1
      fmap (.card.name) ownField `shouldBe` ["Edors Konstruct"]
      case ownField of
        [cardInPlay] -> creatureStrength cardInPlay `shouldBe` 4000
        _ -> expectationFailure "expected exactly one card on the field"
      fmap (.card.name) player1.graveyard `shouldBe` ["Magiestein der Erdkraft"]

    it "returns a card on a foreign field to the card owner's hand" do
      finalState <- runGameActions 1 foreignFieldState [ActivateFromField 1]
      let (activePlayer, opponentPlayer) = finalState.players
      fmap (.card.name) activePlayer.field `shouldBe` []
      fmap (.card.name) activePlayer.graveyard `shouldBe` ["Magiestein der Windkraft"]
      fmap (.card.name) opponentPlayer.hand `shouldBe` ["Edors Konstruct"]

withPlayer1Hand :: [String] -> GameState -> GameState
withPlayer1Hand cardNames state =
  let (player1, player2) = state.players
      chosenCards = zipWith (cardInPlayFor Player1) [1000 ..] $ map lookupCard cardNames
   in state
        { players =
            ( player1{hand = chosenCards, deck = [], field = []}
            , player2{hand = [], deck = [], field = []}
            )
        }

foreignFieldState :: GameState
foreignFieldState =
  let creature = cardInPlayFor Player2 1000 (lookupCard "Edors Konstruct")
      activator = cardInPlayFor Player1 1001 (lookupCard "Magiestein der Windkraft")
      (player1, player2) = initialGameState.players
   in initialGameState
        { players =
            ( player1{hand = [], deck = [], field = [creature, activator], graveyard = []}
            , player2{hand = [], deck = [], field = [], graveyard = []}
            )
        }

cardInPlayFor :: PlayerId -> Int -> Card -> CardInPlay
cardInPlayFor owner idx card =
  CardInPlay{id = CardId idx, owner = owner, card = card, modifications = []}

lookupCard :: String -> Card
lookupCard cardName =
  case find ((== cardName) . (.name)) series26 of
    Just card -> card
    Nothing -> error $ "unknown card: " <> cardName

runGameActions :: Int -> GameState -> [GameAction] -> IO GameState
runGameActions firstChoice gameState actions =
  playGame actions
    & ignoreLog
    & runChoiceInputConst firstChoice
    & execState gameState
    & runEff
