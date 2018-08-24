import Test.Hspec
import Game
import GameIO
import DataTypes
import Control.Lens
import qualified Cards

instance GameIO [] where
    getLine = ["some line"]
    log = \_ -> [()]
    logLn = \_ -> [()]
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
        
        describe "resolving DiscardFromHand" $ do
            it "should remove that card from the active player's hand" $ do
                let newGame = resolve (DiscardFromHand Cards.dog) game :: [GameState]

                (head newGame)^.activePlayer.hand `shouldBe` []
            