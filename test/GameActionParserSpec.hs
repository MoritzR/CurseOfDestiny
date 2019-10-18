module GameActionParserSpec where

import Test.Hspec
import DataTypes
import GameActionParser (parseGameAction)

spec :: Spec
spec = do
    describe "parsing Game Action" $ do
        it "should parse 'p1' to 'PlayFromHand 0'" $
            parseGameAction "p1" `shouldBe` Just (PlayFromHand 0)
        it "should parse 'p7' to 'PlayFromHand 6'" $
            parseGameAction "p7" `shouldBe` Just (PlayFromHand 6)
        it "should parse 'c1' to 'ActivateFromField 0'" $
            parseGameAction "c1" `shouldBe` Just (ActivateFromField 0)
        it "should parse 'd1' to 'AnnounceDirectAttack 0'" $
            parseGameAction "d1" `shouldBe` Just (AnnounceDirectAttack 0)
        it "should parse 'a 1 3' to 'AnnounceAttack 0 2'" $
            parseGameAction "a 1 3" `shouldBe` Just (AnnounceAttack 0 2)
        it "should parse 'pass'" $
            parseGameAction "pass" `shouldBe` Just Pass
        it "should parse 'end'" $
            parseGameAction "end" `shouldBe` Just EndRound
