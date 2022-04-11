module GameSpec where

import Actions (creaturePower, deleteFirst, modifiedField)
import qualified Actions
import qualified Cards
import Control.Lens
import Data.Function ((&))
import DataTypes
import Game (convertGameAction)
import qualified Game
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

combinedField :: GameState -> ([Card], [Card])
combinedField gs = (gs ^. activePlayer . #field, gs ^. enemyPlayer . #field)

expectField :: GameState -> ([Card], [Card]) -> Expectation
expectField gs expectedField = combinedField gs `shouldBe` expectedField

defaultPlayer :: PlayerWithoutId
defaultPlayer = PlayerWithoutId {name = "some player", deck = [], hand = [], field = [], playerCreature = Cards.defaultPlayerCreature}

-- delete

resolveAura :: Aura -> PlayerLens -> GameState -> GameState
resolveAura aura owner gs = case aura of
  IncreaseAttack amount -> gs & owner . #field . traverse %~ applyAura (IncreaseAttack amount)
  DecreaseAttack amount -> gs & owner . otherPlayer gs . #field . traverse %~ applyAura (DecreaseAttack amount)

applyAura :: Aura -> Card -> Card
applyAura aura card = case aura of
  IncreaseAttack amount ->
    over
      #cardType
      ( \case
          Creature power -> Creature (power + amount)
          other -> other
      )
      card
  DecreaseAttack amount ->
    over
      #cardType
      ( \case
          Creature power -> Creature (power - amount)
          other -> other
      )
      card

-- end delete

spec :: Spec
spec = do
  describe "deleteFirst" $ do
    it "should not do anything for an empty list" $
      deleteFirst 1 [] `shouldBe` []
    it "should not do anything for a list that does not conain the element" $
      deleteFirst 1 [2] `shouldBe` [2]
    it "should delete the element in a list with one occurencec" $
      deleteFirst 1 [1, 2] `shouldBe` [2]
    it "should delete the first element in a list with two occurencec" $
      deleteFirst 1 [1, 2, 1] `shouldBe` [2, 1]

  describe "cards equality" $ do
    it "should return true when comparing two dog cards" $
      Cards.dog == Cards.dog `shouldBe` True
    it "should return false when comparing a cat card and a dog card" $
      Cards.cat == Cards.dog `shouldBe` False

  describe "game state interactions" $ do
    let player1 :: PlayerWithoutId = defaultPlayer {name = "player", hand = [Cards.dog]}
    let player2 :: PlayerWithoutId = defaultPlayer {name = "other player", hand = [Cards.dog]}
    let game = newState (player1, player2)

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
        let endTurnPlayer1 :: PlayerWithoutId = defaultPlayer {name = "player", hand = [Cards.dog], field = [Cards.dragonEgg]}
        let endTurnPlayer2 :: PlayerWithoutId = defaultPlayer {name = "other player", hand = [Cards.dog]}
        let endTurnGame = newState (endTurnPlayer1, endTurnPlayer2)
        endTurnGame ^. activePlayer . #name `shouldBe` "player"

        let newGame = resolve EndTurn endTurnGame

        newGame ^. enemyPlayer . #field `shouldBe` [Cards.dragon]

    describe "Buffing/Debuffing" $ do
      let player :: PlayerWithoutId = defaultPlayer {field = [Cards.dog]}
      let game = newState (player, player)

      it "Mr. Buff should increase the power of a dog so it can defeate another dog" $ do
        let newGame =
              game
                & playGame (Play Cards.buff)
                & playGame (AnnounceAttack 0 0)

        newGame ^. activePlayer . #field `shouldBe` [Cards.dog, Cards.buff]
        -- the buffed dog destroys the enemy dog
        game ^. enemyPlayer . #field `shouldBe` [Cards.dog]
        newGame ^. enemyPlayer . #field `shouldBe` []

      it "Mr. Debuff should cancel the effect of Mr.Buff" $ do
        let newGame =
              game
                & playGame (Play Cards.buff)
                & playGame EndRound
                & playGame (Play Cards.debuff)
                & playGame EndRound
                & playGame (AnnounceAttack 0 0)

        -- both dogs destroy eachother, as buff and debuff cancel eachother out
        newGame ^. activePlayer . #field `shouldBe` [Cards.buff]
        newGame ^. enemyPlayer . #field `shouldBe` [Cards.debuff]
        modifiedField activePlayer newGame `shouldBe` [Cards.debuff] -- what is happening here?
        modifiedField enemyPlayer newGame `shouldBe` [Cards.debuff]
        game
          `expectField` ( [Cards.dog],
                          [Cards.dog]
                        )
        newGame
          `expectField` ( [Cards.buff],
                          [Cards.debuff]
                        )

      it "Mr. Debuff should decrease the power of the enemy dog so that it can be defeated by the own dog" $ do
        let newGame =
              game
                & playGame (Play Cards.debuff)
                & playGame (AnnounceAttack 0 0)

        -- the dog destroys the enemy debuffed dog
        -- enemyPlayer activePlayer
        modifiedField enemyPlayer (resolveAura (DecreaseAttack 500) activePlayer game) `shouldBe` []
        -- game
        --   `expectField` ( [Cards.dog],
        --                   [Cards.dog]
        --                 )
        -- newGame
        --   `expectField` ( [Cards.dog, Cards.debuff],
        --                   []
        --                 )

    describe "activating" $ do
      describe "Master of Greed" $ do
        it "should destroy itself when its the only card on the active player's the field" $ do
          let player1 :: PlayerWithoutId = defaultPlayer {field = [Cards.masterOfGreed]}
          let game = newState (player1, player1)
          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #field `shouldBe` []
        it "should destroy another card on the field when the other card is chosen to be destroyed" $ do
          let player1 :: PlayerWithoutId = defaultPlayer {field = [Cards.dog, Cards.masterOfGreed]}
          let game = newState (player1, player1)
          let newGame = playGame (ActivateFromField 1) game

          newGame ^. activePlayer . #field `shouldBe` [Cards.masterOfGreed]
        it "should draw a card for the active player" $ do
          let player1 :: PlayerWithoutId = defaultPlayer {deck = [Cards.dog], field = [Cards.masterOfGreed]}
          let game = newState (player1, player1)
          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #hand `shouldBe` [Cards.dog]
          newGame ^. activePlayer . #deck `shouldBe` []

      describe "catFactory" $ do
        it "should add a cat to the active player's hand" $ do
          let player1 :: PlayerWithoutId = defaultPlayer {field = [Cards.catFactory]}
          let game = newState (player1, player1)

          let newGame = playGame (ActivateFromField 0) game

          newGame ^. activePlayer . #field `shouldSatisfy` elem Cards.cat

    describe "attacking" $ do
      let catPlayer :: PlayerWithoutId = defaultPlayer {name = "catPlayer", field = [Cards.cat]}
      let dogPlayer :: PlayerWithoutId = defaultPlayer {name = "dogPlayer", field = [Cards.dog]}

      context "a cat as a dog" $ do
        let game = newState (catPlayer, dogPlayer)

        it "should destroy the cat" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` []
          newGame ^. enemyPlayer . #field `shouldBe` [Cards.dog]

      context "a dog as a cat" $ do
        let game = newState (dogPlayer, catPlayer)

        it "should destroy the cat" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` [Cards.dog]
          newGame ^. enemyPlayer . #field `shouldBe` []

      context "a cat as a cat" $ do
        let game = newState (catPlayer, catPlayer)

        it "should destroy the both cats" $ do
          let newGame = playGame (AnnounceAttack 0 0) game

          newGame ^. activePlayer . #field `shouldBe` []
          newGame ^. enemyPlayer . #field `shouldBe` []

      context "a player directly" $ do
        let game = newState (catPlayer, catPlayer)

        it "should reduce the attacked players hp by 1" $ do
          let oldHp = game ^. enemyPlayer . playerHp

          let newGame = playGame (AnnounceDirectAttack 0) game

          newGame ^. enemyPlayer . playerHp `shouldBe` (oldHp - 1)