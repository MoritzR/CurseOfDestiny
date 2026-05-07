{-# LANGUAGE OverloadedRecordDot #-}

module GameSpec where

import CardEffect
import Cards (series26)
import Data.Function ((&))
import Data.IORef
import Data.List (find)
import DataTypes
import Effectful (runEff)
import Effectful.State.Static.Local (execState)
import Game
import GameActionParser (GameAction (..))
import GameEffects (ignoreLog, runChoiceInputConst, runChoiceInputIO)
import GameState (initialGameState, playerById)
import Interpreter.Game (creatureStrength, drawOpeningHands)
import Target
import Test.Hspec
import Trigger

spec :: Spec
spec = do
  describe "Game state transitions" $ do
    it "starts with an opening hand drawn from the deck" do
      state <- runDrawOpeningHands initialGameState
      let (player1, player2) = state.players
      length player1.hand `shouldBe` 5
      length player2.hand `shouldBe` 5
      length player1.deck `shouldBe` length series26 - 5
      length player2.deck `shouldBe` length series26 - 5

    it "buffs a played creature when a spell is played afterwards" do
      openingState <- runDrawOpeningHands initialGameState
      state <- runGameActions 1 openingState [PlayFromHand 0, PlayFromHand 0]
      let ownField = cardsForPlayer Player1 state
          (player1, _) = state.players
      length ownField `shouldBe` 1
      fmap (.card.name) player1.graveyard `shouldBe` ["Energieladung"]
      fmap (.card.name) ownField `shouldBe` ["Edors Konstruct"]
      case ownField of
        [cardInPlay] -> creatureStrength cardInPlay `shouldBe` 3000
        _ -> expectationFailure "expected exactly one card on the field"

    it "can activate a permanent card effect that sacrifices itself and buffs a creature" do
      openingState <- runDrawOpeningHands initialGameState
      let state = withPlayer1Hand ["Edors Konstruct", "Magiestein der Erdkraft"] openingState
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

    it "can activate a granted ability from GibFähigkeit" do
      finalState <- runGameActions 1 grantedAbilityState [PlayFromHand 0, PlayFromHand 0, ActivateFromField 0]
      let (player1, _) = finalState.players
      fmap (.card.name) player1.field `shouldBe` ["Schüler der Aktivierung"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Lehrmeister der Aktivierung"]
      fmap (.card.name) player1.hand `shouldBe` ["Energieladung"]

    it "buffs itself when targeting selbst" do
      finalState <- runGameActions 1 selfBuffState [PlayFromHand 0, ActivateFromField 0]
      let ownField = cardsForPlayer Player1 finalState
      fmap (.card.name) ownField `shouldBe` ["Selbststarker Adept"]
      case ownField of
        [cardInPlay] -> creatureStrength cardInPlay `shouldBe` 2000
        _ -> expectationFailure "expected exactly one card on the field"

    it "lets the triggering player choose one target among multiple legal targets" do
      finalState <- runGameActionsWithChoices [2] chooseEnemyState [PlayFromHand 0]
      let opponentField = cardsForPlayer Player2 finalState
          opponent = playerById Player2 finalState
      fmap (.card.name) opponentField `shouldBe` ["Ziel A"]
      fmap (.card.name) opponent.graveyard `shouldBe` ["Ziel B"]

    it "does nothing when choosing an invalid target index" do
      finalState <- runGameActionsWithChoices [99] chooseEnemyState [PlayFromHand 0]
      let opponentField = cardsForPlayer Player2 finalState
          opponent = playerById Player2 finalState
      fmap (.card.name) opponentField `shouldBe` ["Ziel A", "Ziel B"]
      opponent.graveyard `shouldBe` []

    it "destroys a card on a foreign field to the card owner's graveyard" do
      finalState <- runGameActions 1 foreignFieldDestroyState [PlayFromHand 0]
      let activePlayer = playerById Player1 finalState
          opponentPlayer = playerById Player2 finalState
      fmap (.card.name) activePlayer.field `shouldBe` []
      fmap (.card.name) activePlayer.graveyard `shouldBe` ["Feldzerstoerung"]
      fmap (.card.name) opponentPlayer.graveyard `shouldBe` ["Fremder Krieger"]

    it "takes a cheap creature from the graveyard to the hand" do
      finalState <- runGameActions 1 graveyardReturnState [PlayFromHand 0]
      let player1 = playerById Player1 finalState
      fmap (.card.name) player1.hand `shouldBe` ["Junger Held"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Uralter Drache", "Grabruf"]

    it "views the top of the deck and puts the rest under the deck" do
      finalState <- runGameActionsWithChoices [2] deckViewState [PlayFromHand 0]
      let player1 = playerById Player1 finalState
      fmap (.card.name) player1.hand `shouldBe` ["Fund B"]
      fmap (.card.name) player1.deck `shouldBe` ["Fund C", "Fund A"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Blick in die Zukunft"]

    it "vision lets the player reorder chosen cards on top and puts the rest under the deck" do
      finalState <- runGameActionsWithChoices [2, 1, 2] visionState [PlayFromHand 0]
      let player1 = playerById Player1 finalState
      fmap (.card.name) player1.deck `shouldBe` ["Vision B", "Vision A", "Vision D", "Vision C"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Visionstest"]

    it "draws cards based on AnzahlVon" do
      finalState <- runGameActions 1 countDrawState [PlayFromHand 0, PlayFromHand 0, PlayFromHand 0]
      let player1 = playerById Player1 finalState
      fmap (.card.name) player1.field `shouldBe` ["Zaehlwesen A", "Zaehlwesen B"]
      fmap (.card.name) player1.hand `shouldBe` ["Ziehkarte A", "Ziehkarte B"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Zaehlruf"]

    it "draws cards based on AnzahlSchicksalsmächte" do
      finalState <- runGameActions 1 fateDrawState [PlayFromHand 0]
      let player1 = playerById Player1 finalState
      fmap (.card.name) player1.hand `shouldBe` ["Schicksalszug A", "Schicksalszug B"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Schicksalsstudie"]

    it "reuses the chosen target across follow-up effects" do
      finalState <- runGameActionsWithChoices [2, 2] sightUndVerzichtState [PlayFromHand 0, ActivateFromField 0]
      let opponentField = cardsForPlayer Player2 finalState
          chosenTarget = find ((== "Sicht Ziel B") . (.card.name)) opponentField
      fmap (.card.name) opponentField `shouldBe` ["Sicht Ziel A", "Sicht Ziel B"]
      case chosenTarget of
        Just cardInPlay -> do
          creatureStrength cardInPlay `shouldBe` 1000
          cardInPlay.modifications `shouldSatisfy` any isTemporaryGrantedAbility
        Nothing -> expectationFailure "expected to find the chosen target on the field"

    it "can return up to three enemy creatures to the top of their owner's deck" do
      finalState <- runGameActionsWithChoices [2, 2, 2] mutUndFlutState [PlayFromHand 0, ActivateFromField 0]
      let opponent = playerById Player2 finalState
          player1 = playerById Player1 finalState
      fmap (.card.name) opponent.field `shouldBe` []
      fmap (.card.name) opponent.deck `shouldBe` ["Flut Ziel B", "Flut Ziel A"]
      fmap (.card.name) player1.graveyard `shouldBe` ["Magiestein für Mut und Flut"]

    it "destroys the weaker creature for Magiestein für Krieg und Sieg" do
      finalState <- runGameActionsWithChoices [2, 1, 1] kriegUndSiegState [PlayFromHand 0, ActivateFromField 1]
      let player1 = playerById Player1 finalState
          player2 = playerById Player2 finalState
      fmap (.card.name) player1.field `shouldBe` ["Starker Verbündeter"]
      fmap (.card.name) player2.field `shouldBe` []
      fmap (.card.name) player1.graveyard `shouldBe` ["Magiestein für Krieg und Sieg"]
      fmap (.card.name) player2.graveyard `shouldBe` ["Schwacher Gegner"]

    it "switches the current player and removes temporary buffs on endRound" do
      openingState <- runDrawOpeningHands initialGameState
      finalState <- runGameActions 1 openingState [PlayFromHand 0, PlayFromHand 0, EndRound]
      let ownField = cardsForPlayer Player1 finalState
      finalState.currentPlayer `shouldBe` Player2
      case ownField of
        [cardInPlay] -> creatureStrength cardInPlay `shouldBe` 1000
        _ -> expectationFailure "expected exactly one card on the field"

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

selfBuffState :: GameState
selfBuffState = withPlayers (createPlayerState Player1 [selbststarkerAdept] [] []) (createPlayerState Player2 [] [] [])

chooseEnemyState :: GameState
chooseEnemyState =
  let enemyA = cardInPlayFor Player2 1000 (namedCreature "Ziel A" 1000)
      enemyB = cardInPlayFor Player2 1001 (namedCreature "Ziel B" 1000)
   in withPlayers
        (createPlayerState Player1 [zielwahlZauber] [] [])
        (createPlayerState Player2 [] [] [enemyA, enemyB])

foreignFieldDestroyState :: GameState
foreignFieldDestroyState =
  let foreignCreature = cardInPlayFor Player2 1000 (namedCreature "Fremder Krieger" 1000)
   in withPlayers
        (createPlayerState Player1 [feldzerstoerung] [] [foreignCreature])
        (createPlayerState Player2 [] [] [])

graveyardReturnState :: GameState
graveyardReturnState =
  let cheap = cardInPlayFor Player1 1000 (cheapCreature "Junger Held")
      expensive = cardInPlayFor Player1 1001 expensiveCreature
   in withPlayers
        (createPlayerState Player1 [grabruf] [] []){graveyard = [cheap, expensive]}
        (createPlayerState Player2 [] [] [])

deckViewState :: GameState
deckViewState =
  withPlayers
    (createPlayerState Player1 [blickInDieZukunft] [namedSpell "Fund A", namedSpell "Fund B", namedSpell "Fund C"] [])
    (createPlayerState Player2 [] [] [])

visionState :: GameState
visionState =
  withPlayers
    (createPlayerState Player1 [visionstest] [namedSpell "Vision A", namedSpell "Vision B", namedSpell "Vision C", namedSpell "Vision D"] [])
    (createPlayerState Player2 [] [] [])

countDrawState :: GameState
countDrawState =
  withPlayers
    ( createPlayerState
        Player1
        [namedCreature "Zaehlwesen A" 1000, namedCreature "Zaehlwesen B" 1000, zaehlruf]
        [namedSpell "Ziehkarte A", namedSpell "Ziehkarte B"]
        []
    )
    (createPlayerState Player2 [] [] [])

fateDrawState :: GameState
fateDrawState =
  let player1 = createPlayerState Player1 [schicksalsstudie] [namedSpell "Schicksalszug A", namedSpell "Schicksalszug B"] []
      player2 = createPlayerState Player2 [] [] []
   in withPlayers player1{schicksalsmacht = 2} player2

sightUndVerzichtState :: GameState
sightUndVerzichtState =
  let targetA = cardInPlayFor Player2 1000 (namedCreature "Sicht Ziel A" 1000)
      targetB = cardInPlayFor Player2 1001 (namedCreature "Sicht Ziel B" 3000)
   in withPlayers
        (createPlayerState Player1 [lookupCard "Magiestein für Sicht und Verzicht"] [] [])
        (createPlayerState Player2 [] [] [targetA, targetB])

mutUndFlutState :: GameState
mutUndFlutState =
  let targetA = cardInPlayFor Player2 1000 (namedCreature "Flut Ziel A" 1000)
      targetB = cardInPlayFor Player2 1001 (namedCreature "Flut Ziel B" 2000)
   in withPlayers
        (createPlayerState Player1 [lookupCard "Magiestein für Mut und Flut"] [] [])
        (createPlayerState Player2 [] [] [targetA, targetB])

kriegUndSiegState :: GameState
kriegUndSiegState =
  let ownCreature = cardInPlayFor Player1 1000 (namedCreature "Starker Verbündeter" 3000)
      enemyCreature = cardInPlayFor Player2 1001 (namedCreature "Schwacher Gegner" 1000)
   in withPlayers
        (createPlayerState Player1 [lookupCard "Magiestein für Krieg und Sieg"] [] [ownCreature])
        (createPlayerState Player2 [] [] [enemyCreature])

selbststarkerAdept :: Card
selbststarkerAdept =
  Card
    { name = "Selbststarker Adept"
    , cardType = Wesen 1000
    , cost = 1
    , trigger = einmalProRunde do
        erhöhe Stärke selbst Dauerhaft 1000
    , tags = []
    }

zielwahlZauber :: Card
zielwahlZauber =
  Card
    { name = "Zielwahlzauber"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        zerstöre (ein $ gegnerisches <> wesen)
    , tags = []
    }

feldzerstoerung :: Card
feldzerstoerung =
  Card
    { name = "Feldzerstoerung"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        zerstöre (ein $ wesen <> aufDemFeld)
    , tags = []
    }

grabruf :: Card
grabruf =
  Card
    { name = "Grabruf"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        nimmAufDieHand (ein $ wesen <> aufDemFriedHof <> kostetMaximal 3)
    , tags = []
    }

blickInDieZukunft :: Card
blickInDieZukunft =
  Card
    { name = "Blick in die Zukunft"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        schaueObenVomDeck 2 do
          zeigeVorUndNimmtAufDieHand (eine karte)
          legeRestUnterDeck
    , tags = []
    }

visionstest :: Card
visionstest =
  Card
    { name = "Visionstest"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        vision 3
    , tags = []
    }

zaehlruf :: Card
zaehlruf =
  Card
    { name = "Zaehlruf"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        anzahlVon (alle $ eigene <> wesen <> aufDemFeld) ziehe
    , tags = []
    }

schicksalsstudie :: Card
schicksalsstudie =
  Card
    { name = "Schicksalsstudie"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        anzahlSchicksalsmächte Du ziehe
    , tags = []
    }

grantedAbilityState :: GameState
grantedAbilityState =
  let creature = cardInPlayFor Player1 1000 schülerDerAktivierung
      granter = cardInPlayFor Player1 1001 lehrmeisterDerAktivierung
      drawCard = cardInPlayFor Player1 1002 (lookupCard "Energieladung")
      (player1, player2) = initialGameState.players
   in initialGameState
        { players =
            ( player1{hand = [creature, granter], deck = [drawCard], field = [], graveyard = []}
            , player2{hand = [], deck = [], field = [], graveyard = []}
            )
        }

schülerDerAktivierung :: Card
schülerDerAktivierung =
  Card
    { name = "Schüler der Aktivierung"
    , cardType = Wesen 1000
    , cost = 1
    , trigger = keinEffekt
    , tags = []
    }

lehrmeisterDerAktivierung :: Card
lehrmeisterDerAktivierung =
  Card
    { name = "Lehrmeister der Aktivierung"
    , cardType = Allmagie
    , cost = 1
    , trigger = wennGespielt do
        gibFähigkeit (ein $ wesen <> aufDemFeld) Dauerhaft do
          einmalProRunde $ ziehe 1
    , tags = []
    }

cardInPlayFor :: PlayerId -> Int -> Card -> CardInPlay
cardInPlayFor owner idx card =
  CardInPlay{id = CardId idx, owner = owner, card = card, modifications = []}

createPlayerState :: PlayerId -> [Card] -> [Card] -> [CardInPlay] -> Player
createPlayerState owner handCards deckCards fieldCards =
  let basePlayer = playerById owner initialGameState
   in basePlayer
        { hand = zipWith (cardInPlayFor owner) [2000 ..] handCards
        , deck = zipWith (cardInPlayFor owner) [3000 ..] deckCards
        , field = fieldCards
        , graveyard = []
        }

withPlayers :: Player -> Player -> GameState
withPlayers player1 player2 =
  initialGameState
    { players = (player1, player2)
    , currentPlayer = Player1
    }

namedCreature :: String -> Int -> Card
namedCreature creatureName strength =
  Card
    { name = creatureName
    , cardType = Wesen strength
    , cost = 1
    , trigger = keinEffekt
    , tags = []
    }

cheapCreature :: String -> Card
cheapCreature creatureName =
  Card
    { name = creatureName
    , cardType = Wesen 1000
    , cost = 3
    , trigger = keinEffekt
    , tags = []
    }

expensiveCreature :: Card
expensiveCreature =
  Card
    { name = "Uralter Drache"
    , cardType = Wesen 5000
    , cost = 5
    , trigger = keinEffekt
    , tags = []
    }

namedSpell :: String -> Card
namedSpell spellName =
  Card
    { name = spellName
    , cardType = Allmagie
    , cost = 1
    , trigger = keinEffekt
    , tags = []
    }

lookupCard :: String -> Card
lookupCard cardName =
  case find ((== cardName) . (.name)) series26 of
    Just card -> card
    Nothing -> error $ "unknown card: " <> cardName

isTemporaryGrantedAbility :: Modification -> Bool
isTemporaryGrantedAbility = \case
  FähigkeitsModifikation BisZumEndeDesZuges _ ->
    True
  _ -> False

cardsForPlayer :: PlayerId -> GameState -> [CardInPlay]
cardsForPlayer owner state = (playerById owner state).field

runGameActions :: Int -> GameState -> [GameAction] -> IO GameState
runGameActions firstChoice gameState actions =
  playGame actions
    & ignoreLog
    & runChoiceInputConst firstChoice
    & execState gameState
    & runEff

runDrawOpeningHands :: GameState -> IO GameState
runDrawOpeningHands gameState =
  drawOpeningHands
    & execState gameState
    & runEff

runGameActionsWithChoices :: [Int] -> GameState -> [GameAction] -> IO GameState
runGameActionsWithChoices choices gameState actions = do
  choicesRef <- newIORef choices
  let nextChoice = do
        remainingChoices <- readIORef choicesRef
        case remainingChoices of
          [] -> pure 1
          choice : rest -> writeIORef choicesRef rest >> pure choice
  playGame actions
    & ignoreLog
    & runChoiceInputIO nextChoice
    & execState gameState
    & runEff
