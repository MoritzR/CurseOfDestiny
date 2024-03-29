module GameSpec where

import Actions (creaturePower)
import Actions qualified
import Cards qualified
import Control.Lens
import Data.Function ((&))
import DataTypes
import Game (convertGameAction)
import Game qualified
import GameIO
import Polysemy (Sem, run)
import Polysemy.Input (Input, runInputConst)
import Polysemy.State (State, execState)
import Polysemy.Trace (Trace, ignoreTrace)
import Test.Hspec

runForTests :: GameState -> Sem '[State GameState, Input Int, Trace] a -> GameState
runForTests gs sem =
  sem
    & execState gs
    & runInputConst 1
    & ignoreTrace
    & run

playGame :: GameAction -> GameState -> GameState
playGame action gs = Game.playGame (convertGameAction action gs) & runForTests gs

resolve :: Action -> GameState -> GameState
resolve action gs = Actions.resolve action & runForTests gs

defaultPlayer = Player {name = "some player", deck = [], hand = [], field = [], playerCreature = Cards.defaultPlayerCreature}

spec :: Spec
spec = do
  describe "cards equality" $ do
    it "should return true when comparing two dog cards" $
      Cards.dog == Cards.dog `shouldBe` True
    it "should return false when comparing a cat card and a dog card" $
      Cards.cat == Cards.dog `shouldBe` False

  describe "game state interactions" $ do
    let player1 = defaultPlayer {name = "player", hand = [Cards.dog]}
    let player2 = defaultPlayer {name = "other player", hand = [Cards.dog]}
    let game = GameState (player1, player2)

    describe "playing a creature" $ do
      it "should add that creature to the field of the active player" $ do
        let newGame = playGame (PlayFromHand 0) game

        newGame ^. activePlayer . #field `shouldBe` [Cards.dog]

      it "should remove that creatue from the active player's hand" $ do
        let newGame = playGame (PlayFromHand 0) game

        newGame ^. activePlayer . #hand `shouldBe` []

    describe "resolving DiscardFromHand" $ do
      it "should remove that card from the active player's hand" $ do
        let newGame = resolve (DiscardFromHand Cards.dog) game

        newGame ^. activePlayer . #hand `shouldBe` []

    describe "ending the turn" $ do
      it "should switch the current active player" $ do
        game ^. activePlayer . #name `shouldBe` "player"

        let newGame = resolve EndTurn game

        newGame ^. activePlayer . #name `shouldBe` "other player"

      it "should evolve the dragonEgg to a dragon" $ do
        let endTurnPlayer1 = defaultPlayer {name = "player", hand = [Cards.dog], field = [Cards.dragonEgg]}
        let endTurnPlayer2 = defaultPlayer {name = "other player", hand = [Cards.dog]}
        let endTurnGame = GameState (endTurnPlayer1, endTurnPlayer2)
        endTurnGame ^. activePlayer . #name `shouldBe` "player"

        let newGame = resolve EndTurn endTurnGame

        newGame ^. enemyPlayer . #field `shouldBe` [Cards.dragon]

    describe "Buff" $ do
      it "should increase the power of a dog so it can defeate another dog" $ do
        let player1 = defaultPlayer {field = [Cards.dog, Cards.buff]}
        let player2 = defaultPlayer {field = [Cards.dog]}
        let game = GameState (player1, player2)
        let newGame = playGame (AnnounceAttack 0 0) game

        newGame ^. activePlayer . #field `shouldBe` [Cards.dog, Cards.buff]
        -- the buffed dog destroys the enemy dog
        game ^. enemyPlayer . #field `shouldBe` [Cards.dog]
        newGame ^. enemyPlayer . #field `shouldBe` []

    describe "activating" $ do
      describe "Master of Greed" $ do
        it "should destroy itself when its the only card on the active player's the field" $ do
          let player1 = defaultPlayer {field = [Cards.masterOfGreed]}
          let game = GameState (player1, player1)
          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #field `shouldBe` []
        it "should destroy another card on the field when the other card is chosen to be destroyed" $ do
          let player1 = defaultPlayer {field = [Cards.dog, Cards.masterOfGreed]}
          let game = GameState (player1, player1)
          let newGame = playGame (ActivateFromField 1) game

          newGame ^. activePlayer . #field `shouldBe` [Cards.masterOfGreed]
        it "should draw a card for the active player" $ do
          let player1 = defaultPlayer {deck = [Cards.dog], field = [Cards.masterOfGreed]}
          let game = GameState (player1, player1)
          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #hand `shouldBe` [Cards.dog]
          newGame ^. activePlayer . #deck `shouldBe` []

      describe "catFactory" $ do
        it "should add a cat to the active player's hand" $ do
          let player1 = defaultPlayer {field = [Cards.catFactory]}
          let game = GameState (player1, player1)

          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #field `shouldSatisfy` elem Cards.cat

    describe "attacking" $ do
      let catPlayer = defaultPlayer {name = "catPlayer", field = [Cards.cat]}
      let dogPlayer = defaultPlayer {name = "dogPlayer", field = [Cards.dog]}

      context "a cat as a dog" $ do
        let game = GameState (catPlayer, dogPlayer)

        it "should destroy the cat" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` []
          newGame ^. enemyPlayer . #field `shouldBe` [Cards.dog]

      context "a dog as a cat" $ do
        let game = GameState (dogPlayer, catPlayer)

        it "should destroy the cat" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` [Cards.dog]
          newGame ^. enemyPlayer . #field `shouldBe` []

      context "a cat as a cat" $ do
        let game = GameState (catPlayer, catPlayer)

        it "should destroy the both cats" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` []
          newGame ^. enemyPlayer . #field `shouldBe` []

      context "a player directly" $ do
        let game = GameState (catPlayer, catPlayer)

        it "should reduce the attacked players hp by 1" $ do
          let oldHp = game ^. enemyPlayer . playerHp

          let newGame = playGame (AnnounceDirectAttack 0) game

          newGame ^. enemyPlayer . playerHp `shouldBe` (oldHp - 1)