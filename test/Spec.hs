import Test.Hspec
import Game
import GameIO
import DataTypes
import Control.Lens
import qualified Cards

instance GameIO [] where
    getLine = return "some line"
    log = \_ -> return ()
    logLn = \_ -> return ()
    chooseOne = return . Just . head

main :: IO ()
main = hspec $ do
    describe "card effects filters" $ do
        it "filter on play effects" $
            onPlayEffects [OnPlay $ Choose [EndTurn], OnTurnEnd EndTurn] `shouldBe` [Choose [EndTurn]]

        it "filter on turn end effects" $
            onTurnEndEffects [OnPlay $ Choose [EndTurn], OnTurnEnd EndTurn] `shouldBe` [EndTurn]
        
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
        let player1 = Player {_name = "player", _deck = [], _hand = [Cards.dog], _field = []}
        let player2 = Player {_name = "other player", _deck = [], _hand = [Cards.dog], _field = []}
        let game = GameState (player1,player2)

        describe "playing a creature" $ do
            it "should add that creature to the field of the active player" $ do
                let newGame = playGame (convertGameAction (PlayFromHand 0) game) game :: [GameState]

                (head newGame)^.activePlayer.field `shouldBe` [Cards.dog]

            it "should remove that creatue from the active player's hand" $ do
                let newGame = playGame (convertGameAction (PlayFromHand 0) game) game :: [GameState]

                (head newGame)^.activePlayer.hand `shouldBe` []

        describe "activating the 'Master of Greed'" $ do
            it "should destroy itself when its the only card on the active player's the field" $ do
                let player1 = Player {_name = "player", _deck = [], _hand = [], _field = [Cards.masterOfGreed]}
                let game = GameState (player1, player1)
                let newGame = playGame (convertGameAction (ActivateFromField 0) game) game :: [GameState]

                (head newGame)^.activePlayer.field `shouldBe` []
            it "should destroy another card on the field when the other card is chosen to be destroyed" $ do
                let player1 = Player {_name = "player", _deck = [], _hand = [], _field = [Cards.dog, Cards.masterOfGreed]}
                let game = GameState (player1, player1)
                let newGame = playGame (convertGameAction (ActivateFromField 1) game) game :: [GameState]

                (head newGame)^.activePlayer.field `shouldBe` [Cards.masterOfGreed]

        describe "resolving DiscardFromHand" $ do
            it "should remove that card from the active player's hand" $ do
                let newGame = resolve (DiscardFromHand Cards.dog) game :: [GameState]

                (head newGame)^.activePlayer.hand `shouldBe` []

        describe "activating" $ do
            describe "catFactory" $ do
                it "should add a cat to the active player's hand" $ do
                    let player1 = Player {_name = "player", _deck = [], _hand = [], _field = [Cards.catFactory]}
                    let game = GameState (player1, player1)

                    let newGame = playGame (convertGameAction (ActivateFromField 0) game) game :: [GameState]
    
                    (head newGame)^.activePlayer.field `shouldSatisfy` elem Cards.cat

        describe "attacking" $ do
            let catPlayer = Player {_name = "catPlayer", _deck = [], _hand = [], _field = [Cards.cat]}
            let dogPlayer = Player {_name = "dogPlayer", _deck = [], _hand = [], _field = [Cards.dog]}

            context "a cat as a dog" $ do
                let game = GameState (catPlayer, dogPlayer)

                it "should destroy the cat" $ do
                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game :: [GameState]
    
                    (head newGame)^.activePlayer.field `shouldBe` []
                    (head newGame)^.enemyPlayer.field `shouldBe` [Cards.dog]

            context "a dog as a cat" $ do
                let game = GameState (dogPlayer, catPlayer)

                it "should destroy the cat" $ do

                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game :: [GameState]
    
                    (head newGame)^.activePlayer.field `shouldBe` [Cards.dog]
                    (head newGame)^.enemyPlayer.field `shouldBe` []
            context "a cat as a cat" $ do
                let game = GameState (catPlayer, catPlayer)

                it "should destroy the both cats" $ do

                    let newGame = playGame (convertGameAction (AnnounceAttack 0 0) game) game :: [GameState]
    
                    (head newGame)^.activePlayer.field `shouldBe` []
                    (head newGame)^.enemyPlayer.field `shouldBe` []
        