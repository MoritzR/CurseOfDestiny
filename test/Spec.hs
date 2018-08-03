import Test.Hspec
import Game
import DataTypes

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