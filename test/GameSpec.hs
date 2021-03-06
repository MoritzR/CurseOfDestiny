module GameSpec where
import Test.Hspec
import Game (convertGameAction)
import qualified Game
import Actions (deleteFirst)
import qualified Actions
import GameIO
import DataTypes
import Control.Lens
import qualified Cards
import Polysemy (run, Sem)
import Polysemy.Trace (Trace, ignoreTrace)
import Polysemy.Input (Input, runInputConst)
import Polysemy.State (State, execState)
import Data.Function ((&))

runForTests ::  GameState -> Sem '[State GameState, Input Int, Trace] a -> GameState
runForTests gs sem = sem
    & execState gs
    & runInputConst 1
    & ignoreTrace
    & run

playGame :: [Action] -> GameState -> GameState
playGame actions gs = Game.playGame actions & runForTests gs

resolve :: Action -> GameState -> GameState
resolve action gs = Actions.resolve action & runForTests gs

defaultPlayer = Player {name = "some player", deck = [], hand = [], field = [], playerCreature = Cards.defaultPlayerCreature}

spec :: Spec
spec = do
    describe "deleteFirst" $ do
        it "should not do anything for an empty list" $
            deleteFirst 1 [] `shouldBe` []
        it "should not do anything for a list that does not conain the element" $
            deleteFirst 1 [2] `shouldBe` [2]
        it "should delete the element in a list with one occurencec" $
            deleteFirst 1 [1,2] `shouldBe` [2]
        it "should delete the first element in a list with two occurencec" $
            deleteFirst 1 [1,2,1] `shouldBe` [2,1]
    
    describe "cards equality" $ do
        it "should return true when comparing two dog cards" $
            Cards.dog == Cards.dog `shouldBe` True
        it "should return false when comparing a cat card and a dog card" $
            Cards.cat == Cards.dog `shouldBe` False
    
    describe "game state interactions" $ do
        let player1 = defaultPlayer {name = "player", hand = [Cards.dog]}
        let player2 = defaultPlayer {name = "other player", hand = [Cards.dog]}
        let game = GameState (player1,player2)

        describe "playing a creature" $ do
            it "should add that creature to the field of the active player" $ do
                let newGame = playGame (convertGameAction (PlayFromHand 0) game) game

                newGame^.activePlayer. #field `shouldBe` [Cards.dog]

            it "should remove that creatue from the active player's hand" $ do
                let newGame = playGame (convertGameAction (PlayFromHand 0) game) game

                newGame^.activePlayer. #hand `shouldBe` []

        describe "resolving DiscardFromHand" $ do
            it "should remove that card from the active player's hand" $ do
                let newGame = resolve (DiscardFromHand Cards.dog) game

                newGame^.activePlayer. #hand `shouldBe` []

        describe "ending the turn" $ do
            it "should switch the current active player" $ do
                game^.activePlayer. #name `shouldBe` "player"

                let newGame = resolve EndTurn game

                newGame^.activePlayer. #name `shouldBe` "other player"

            it "should evolve the dragonEgg to a dragon" $ do
                let endTurnPlayer1 = defaultPlayer {name = "player", hand = [Cards.dog], field = [Cards.dragonEgg]}
                let endTurnPlayer2 = defaultPlayer {name = "other player", hand = [Cards.dog]}
                let endTurnGame = GameState (endTurnPlayer1, endTurnPlayer2)
                endTurnGame^.activePlayer. #name `shouldBe` "player"

                let newGame = resolve EndTurn endTurnGame

                newGame^.enemyPlayer. #field `shouldBe` [Cards.dragon]

        -- describe "playing" $ do
        --     describe "Buff" $ do
        --         it "should increase the power of dog from 1500 to 2500" $ do
        --             let player1 = defaultPlayer {field = [Cards.dog], hand = [Cards.buff]}
        --             let game = GameState (player1, player1)
        --             let newGame = playGame (convertGameAction (PlayFromHand 0) game) game

        --             newGame^.activePlayer. #field `shouldBe` [Cards.dog^.power.~2500]

        describe "activating" $ do
            describe "Master of Greed" $ do
                it "should destroy itself when its the only card on the active player's the field" $ do
                    let player1 = defaultPlayer {field = [Cards.masterOfGreed]}
                    let game = GameState (player1, player1)
                    let newGame = playGame (convertGameAction (ActivateFromField 0) game) game
    
                    newGame^.activePlayer. #field `shouldBe` []
                it "should destroy another card on the field when the other card is chosen to be destroyed" $ do
                    let player1 = defaultPlayer { field = [Cards.dog, Cards.masterOfGreed]}
                    let game = GameState (player1, player1)
                    let newGame = playGame (convertGameAction (ActivateFromField 1) game) game
    
                    newGame^.activePlayer. #field `shouldBe` [Cards.masterOfGreed]
                it "should draw a card for the active player" $ do
                    let player1 = defaultPlayer { deck = [Cards.dog], field = [Cards.masterOfGreed]}
                    let game = GameState (player1, player1)
                    let newGame = playGame (convertGameAction (ActivateFromField 0) game) game
    
                    newGame^.activePlayer. #hand `shouldBe` [Cards.dog]
                    newGame^.activePlayer. #deck `shouldBe` []

            describe "catFactory" $ do
                it "should add a cat to the active player's hand" $ do
                    let player1 = defaultPlayer {field = [Cards.catFactory]}
                    let game = GameState (player1, player1)

                    let newGame = playGame (convertGameAction (ActivateFromField 0) game) game
    
                    newGame^.activePlayer. #field `shouldSatisfy` elem Cards.cat

        describe "attacking" $ do
            let catPlayer = defaultPlayer {name = "catPlayer", field = [Cards.cat]}
            let dogPlayer = defaultPlayer {name = "dogPlayer", field = [Cards.dog]}

            context "a cat as a dog" $ do
                let game = GameState (catPlayer, dogPlayer)

                it "should destroy the cat" $ do
                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game
    
                    newGame^.activePlayer. #field `shouldBe` []
                    newGame^.enemyPlayer. #field `shouldBe` [Cards.dog]

            context "a dog as a cat" $ do
                let game = GameState (dogPlayer, catPlayer)

                it "should destroy the cat" $ do
                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game
    
                    newGame^.activePlayer. #field `shouldBe` [Cards.dog]
                    newGame^.enemyPlayer. #field `shouldBe` []
                    
            context "a cat as a cat" $ do
                let game = GameState (catPlayer, catPlayer)

                it "should destroy the both cats" $ do
                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game
    
                    newGame^.activePlayer. #field `shouldBe` []
                    newGame^.enemyPlayer. #field `shouldBe` []
            
            context "a player directly" $ do
                let game = GameState (catPlayer, catPlayer)

                it "should reduce the attacked players hp by 1" $ do
                    let oldHp = game^.enemyPlayer.playerHp

                    let newGame = playGame (convertGameAction (AnnounceDirectAttack 0) game) game

                    newGame^.enemyPlayer.playerHp `shouldBe` (oldHp - 1)
                    