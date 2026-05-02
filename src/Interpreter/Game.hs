module Interpreter.Game (
  creatureStrength,
  playCardFromHand,
  activateCardOnField,
  runOnPlayTrigger,
  collectActivations,
  runEffect,
  removeTemporaryModifications,
  drawOpeningHands,
) where

import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Free (Free (..), foldFree, iter, iterM)
import Data.Foldable (fold)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, maybeToList)
import DataTypes
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets, modify)
import EffectfulLens ((++=))
import Element (gesamtKosten)
import GameEffects (ChoiceInput, Log)
import GameIO qualified as Gio
import GameState (currentPlayer, getGameState, opponentPlayer)
import Optics (AffineTraversal', (%))
import Optics.AffineTraversal (unsafeFiltered)
import Optics.Label ()
import Target (ein, ownerOfTriggeringCard, wesen)

type HasStateIO es = (State GameState :> es, ChoiceInput :> es, Log :> es)

data LocatedCard = LocatedCard {locationOwner :: PlayerId, cardInPlay :: CardInPlay, location :: Location}
  deriving (Eq, Show)

data Location = Hand | Graveyard | Field
  deriving (Eq, Show)

creatureStrength :: CardInPlay -> Int
creatureStrength cardInPlay = baseStrength cardInPlay.card + sum (strengthDelta <$> cardInPlay.modifications)

playCardFromHand :: HasStateIO r => Int -> Eff r ()
playCardFromHand index = do
  activePlayer <- gets currentPlayer
  maybeCard <- removeFromHand activePlayer index
  case maybeCard of
    Nothing ->
      Gio.logLn' "Keine Karte auf diesem Hand-Slot."
    Just card -> do
      runOnPlayTrigger card
      if isPermanent card.card.cardType
        then void $ addCardToField activePlayer card
        else addToGraveyard activePlayer.playerId [card]

isPermanent :: CardType -> Bool
isPermanent = \case
  Wesen{}; MagieDauerhaft -> True
  Magie; Allmagie; Gegenmagie -> False

activateCardOnField :: HasStateIO r => Int -> Eff r ()
activateCardOnField index = do
  activePlayer <- gets currentPlayer
  case atMay activePlayer.field index of
    Nothing ->
      Gio.logLn' "Keine Karte auf diesem Feld-Slot."
    Just source -> do
      let activations = collectActivations $ effectiveTrigger source
      case activations of
        [] ->
          Gio.logLn' "Diese Karte hat keine aktivierbaren Effekte."
        [activation] ->
          runEffect source.id activation
        _ -> do
          Gio.logLn' "Waehle einen Effekt:"
          choice <- Gio.chooseOne [1 .. length activations]
          maybe (pure ()) (\picked -> runEffect source.id (activations !! (picked - 1))) choice

runOnPlayTrigger :: HasStateIO r => CardInPlay -> Eff r ()
runOnPlayTrigger source =
  iterM (\instruction -> runPlayTriggerInstruction source instruction *> sequence_ instruction) (effectiveTrigger source)

runPlayTriggerInstruction :: HasStateIO r => CardInPlay -> TriggerInstruction (Eff r ()) -> Eff r ()
runPlayTriggerInstruction source = \case
  WennGespielt effect _ -> runEffect source.id effect
  _ -> pure ()

collectActivations :: Trigger -> [CardEffect]
collectActivations = iter collectActivation . fmap (const [])
 where
  collectActivation = \case
    Zahle _ effect next -> effect : next
    EinmalProRunde effect next -> effect : next
    instruction -> fold instruction

runEffect :: HasStateIO r => CardId -> CardEffect -> Eff r ()
runEffect sourceId = foldFree (runInstruction sourceId)

runInstruction :: HasStateIO r => CardId -> Instruction a -> Eff r a
runInstruction sourceId = \case
  Ziehe anzahl next -> do
    drawCardsForCurrentPlayer (anzahlToInt anzahl)
    pure next
  Erhöhe wert ziel dauer höhe next -> do
    increaseValue sourceId wert ziel dauer (anzahlToInt höhe)
    pure next
  Vision _ next ->
    pure next
  Prisma effectForX next -> do
    runEffect sourceId (effectForX 0)
    pure next
  Spende _ _ next ->
    pure next
  WähleAus options effectForOption next -> do
    choice <- Gio.chooseOne options
    maybe (pure ()) (runEffect sourceId . effectForOption) choice
    pure next
  WähleEffekt effects next -> do
    choice <- Gio.chooseOne [1 .. length effects]
    maybe (pure ()) (\picked -> runEffect sourceId (effects !! (picked - 1))) choice
    pure next
  Opfere ziel next -> do
    sacrificeTargets sourceId ziel
    pure next
  Heile anzahl next -> do
    modifyCurrentPlayer \player -> player{schicksalsmacht = player.schicksalsmacht + anzahlToInt anzahl}
    pure next
  GibAufDieHandZurück ziel next -> do
    bounceTargets sourceId ziel
    pure next
  Zerstöre ziel next -> do
    destroyTargets sourceId ziel
    pure next
  Verringere wert ziel dauer höhe next -> do
    increaseValue sourceId wert ziel dauer (negate $ anzahlToInt höhe)
    pure next
  VerringereUndZerstöre ziel dauer höhe next -> do
    increaseValue sourceId Stärke ziel dauer (negate $ anzahlToInt höhe)
    destroyDeadCreatures
    pure next
  NimmAufDieHand ziel next -> do
    takeTargetsToHand sourceId ziel
    pure next
  ZeigeObenVomDeck anzahl lesbarerWert effectForX next -> do
    value <- readTopOfDeckValue (anzahlToInt anzahl) lesbarerWert
    runEffect sourceId (effectForX value)
    pure next
  BringeInsSpiel card next -> do
    activePlayer <- gets currentPlayer
    _ <- putNewCardOnField activePlayer card
    pure next
  BringeInsSpielAusZiel ziel next -> do
    bringTargetIntoPlay sourceId ziel
    pure next
  GibFähigkeit ziel dauer triggerInstrs next -> do
    addAbilityToTargets sourceId ziel dauer triggerInstrs
    pure next
  EinSpielerOpfertEinWesen next -> do
    sacrificeTargets sourceId (ein wesen)
    pure next
  AnzahlVon ziel effectForAmount next -> do
    amount <- countTargets sourceId ziel
    runEffect sourceId (effectForAmount amount)
    pure next
  WirfAb anzahl _ next -> do
    discardFromCurrentHand (anzahlToInt anzahl)
    pure next
  LegeVomDeckAufDenFriedhof anzahl _ next -> do
    millCurrentDeck (anzahlToInt anzahl)
    pure next
  SchaueObenVomDeck anzahl instructions next -> do
    inspectTopOfDeck (anzahlToInt anzahl) instructions
    pure next
  SiehHandkartenAnUndEntferneEineAusDemSpiel next ->
    pure next
  BringeKopieInsSpiel ziel next -> do
    copyTargetIntoPlay sourceId ziel
    pure next
  AnzahlSchicksalsMächte spielerZiel effectForAmount next -> do
    amount <- readSchicksalsmächte spielerZiel
    runEffect sourceId (effectForAmount amount)
    pure next

increaseValue :: HasStateIO r => CardId -> Wert -> Ziel -> Dauer -> Int -> Eff r ()
increaseValue sourceId Stärke ziel dauer höhe = do
  targets <- selectTargets sourceId ziel
  allCards % targeted targets % #modifications ++= [StärkeModifikation dauer höhe]

targeted :: [CardInPlay] -> AffineTraversal' CardInPlay CardInPlay
targeted targets = unsafeFiltered (\card -> card.id `elem` fmap (.id) targets)

sacrificeTargets :: HasStateIO r => CardId -> Ziel -> Eff r ()
sacrificeTargets sourceId ziel = do
  targets <- selectTargets sourceId ziel
  mapM_ sacrificeLocatedCard targets

destroyTargets :: HasStateIO r => CardId -> Ziel -> Eff r ()
destroyTargets sourceId ziel = do
  targets <- selectTargets sourceId ziel
  mapM_ destroyLocatedCard targets

bounceTargets :: HasStateIO r => CardId -> Ziel -> Eff r ()
bounceTargets sourceId ziel = do
  targets <- selectTargets sourceId ziel
  mapM_ returnCardToHand targets

takeTargetsToHand :: HasStateIO r => CardId -> Ziel -> Eff r ()
takeTargetsToHand sourceId ziel = do
  targets <- selectTargets sourceId ziel
  mapM_ takeLocatedCardToCurrentHand targets

bringTargetIntoPlay :: HasStateIO r => CardId -> Ziel -> Eff r ()
bringTargetIntoPlay sourceId ziel = do
  targets <- selectTargets sourceId ziel
  activePlayer <- gets currentPlayer
  forM_ targets \target -> do
    maybeCard <- removeCards [target.id]
    mapM_ (addCardToField activePlayer) maybeCard

copyTargetIntoPlay :: HasStateIO r => CardId -> Ziel -> Eff r ()
copyTargetIntoPlay sourceId ziel = do
  activePlayer <- gets currentPlayer
  targets <- selectTargets sourceId ziel
  mapM_ (putNewCardOnField activePlayer) (fmap (.card) targets)

addAbilityToTargets :: HasStateIO r => CardId -> Ziel -> Dauer -> Trigger -> Eff r ()
addAbilityToTargets sourceId ziel dauer triggerInstrs = do
  targets <- selectTargets sourceId ziel
  allCards % targeted targets % #modifications ++= [FähigkeitsModifikation dauer triggerInstrs]

countTargets :: HasStateIO r => CardId -> Ziel -> Eff r Anzahl
countTargets sourceId ziel = Actual . length <$> selectableTargets sourceId ziel.ziel

readTopOfDeckValue :: HasStateIO r => Int -> LesbarerWert -> Eff r Anzahl
readTopOfDeckValue n LesbarKosten = do
  activePlayer <- gets currentPlayer
  let topCards = take n activePlayer.deck
  pure $ Actual $ sum (gesamtKosten . (.card.cost) <$> topCards)

readSchicksalsmächte :: HasStateIO r => SpielerZiel -> Eff r Anzahl
readSchicksalsmächte spielerZiel = do
  activePlayer <- gets currentPlayer
  targetPlayer <- case spielerZiel of
    Du -> pure activePlayer
    Gegner -> gets opponentPlayer
  pure $ Actual targetPlayer.schicksalsmacht

discardFromCurrentHand :: HasStateIO r => Int -> Eff r ()
discardFromCurrentHand n = replicateM_ n do
  activePlayer <- gets currentPlayer
  case activePlayer.hand of
    [] -> pure ()
    cards -> do
      choice <- Gio.chooseOne $ zip [1 ..] cards
      case choice of
        Nothing -> pure ()
        Just (pickedIndex, _) -> do
          maybeCard <- removeFromHand activePlayer (pickedIndex - 1)
          addToGraveyard activePlayer.playerId (maybeToList maybeCard)

millCurrentDeck :: HasStateIO r => Int -> Eff r ()
millCurrentDeck n = do
  activePlayer <- gets currentPlayer
  let (milled, restDeck) = splitAt n activePlayer.deck
  modifyPlayer activePlayer.playerId \current ->
    current{deck = restDeck, graveyard = current.graveyard <> milled}

inspectTopOfDeck :: HasStateIO r => Int -> InstructionWhenViewingDeckF () -> Eff r ()
inspectTopOfDeck n instructions = do
  activePlayer <- gets currentPlayer
  let (viewedCards, restDeck) = splitAt n activePlayer.deck
  modifyPlayer activePlayer.playerId \current -> current{deck = restDeck}
  remainingCards <- runViewedInstructions activePlayer.playerId viewedCards instructions
  modifyPlayer activePlayer.playerId \current -> current{deck = remainingCards <> current.deck}

runViewedInstructions :: HasStateIO r => PlayerId -> [CardInPlay] -> InstructionWhenViewingDeckF () -> Eff r [CardInPlay]
runViewedInstructions _ viewedCards (Pure ()) = pure viewedCards
runViewedInstructions playerId viewedCards (Free instruction) = case instruction of
  ZeigeVorUndNimmAufDieHand ziel next -> do
    remaining <- moveViewedCard viewedCards ziel \card -> modifyPlayer playerId \player -> player{hand = player.hand <> [card]}
    runViewedInstructions playerId remaining next
  ZeigeVorUndWirfAb ziel next -> do
    remaining <- moveViewedCard viewedCards ziel \card -> modifyPlayer playerId \player -> player{graveyard = player.graveyard <> [card]}
    runViewedInstructions playerId remaining next
  LegeRestUnterDasDeck next -> do
    modifyPlayer playerId \player -> player{deck = player.deck <> viewedCards}
    runViewedInstructions playerId [] next
  WähleVomDeck options next -> do
    choice <- Gio.chooseOne [1 .. length options]
    afterChoice <- case choice of
      Nothing -> pure viewedCards
      Just picked -> runViewedInstructions playerId viewedCards (options !! (picked - 1))
    runViewedInstructions playerId afterChoice next

moveViewedCard :: HasStateIO r => [CardInPlay] -> Ziel -> (CardInPlay -> Eff r ()) -> Eff r [CardInPlay]
moveViewedCard viewedCards ziel onMove = do
  let options = filter (matchesViewedCard ziel) viewedCards
  case options of
    [] -> pure viewedCards
    [singleCard] -> do
      onMove singleCard
      pure $ removeFirstById singleCard.id viewedCards
    _ -> do
      choice <- Gio.chooseOne options
      case choice of
        Nothing -> pure viewedCards
        Just card -> do
          onMove card
          pure $ removeFirstById card.id viewedCards

selectTargets :: HasStateIO r => CardId -> Ziel -> Eff r [CardInPlay]
selectTargets sourceId ziel = do
  choices <- selectableTargets sourceId ziel.ziel
  state <- getGameState
  let triggeringPlayer = ownerOfTriggeringCard state sourceId
  case ziel.anzahl of
    Alle; Undefiniert -> pure choices
    Ein; Eine -> case choices of
      [] -> pure []
      _ -> maybeToList <$> chooseTargetFor triggeringPlayer choices

chooseTargetFor :: (Log :> es, ChoiceInput :> es, Show a) => PlayerId -> [a] -> Eff es (Maybe a)
chooseTargetFor playerId choices = do
  Gio.logLn' $ show playerId <> " needs to choose."
  Gio.chooseOne choices

selectableTargets :: HasStateIO r => CardId -> EinZiel -> Eff r [CardInPlay]
selectableTargets sourceId ziel = do
  state <- getGameState
  pure $ ziel.candidates state sourceId

removeTemporaryModifications :: CardInPlay -> CardInPlay
removeTemporaryModifications cardInPlay =
  cardInPlay{modifications = filter isPermanentModification cardInPlay.modifications}
 where
  isPermanentModification = \case
    StärkeModifikation Dauerhaft _ -> True
    FähigkeitsModifikation Dauerhaft _ -> True
    _ -> False

destroyDeadCreatures :: HasStateIO r => Eff r ()
destroyDeadCreatures = do
  fieldCards <- gets fieldCardsForTarget
  let fieldCardsInPlay = fmap (.cardInPlay) fieldCards
  let deadCards = filter isDeadCreature fieldCardsInPlay
  mapM_ destroyLocatedCard deadCards

isDeadCreature :: CardInPlay -> Bool
isDeadCreature cardInPlay = case cardInPlay.card.cardType of
  Wesen _ _ -> creatureStrength cardInPlay <= 0
  _ -> False

baseStrength :: Card -> Int
baseStrength card = case card.cardType of
  Wesen _ strength -> strength
  _ -> 0

strengthDelta :: Modification -> Int
strengthDelta = \case
  StärkeModifikation _ delta -> delta
  FähigkeitsModifikation _ _ -> 0

effectiveTrigger :: CardInPlay -> Trigger
effectiveTrigger cardInPlay =
  cardInPlay.card.trigger >> grantedTrigger
 where
  grantedTrigger =
    foldr (>>) (pure ()) [trigger | FähigkeitsModifikation _ trigger <- cardInPlay.modifications]

fieldCardsForTarget :: GameState -> [LocatedCard]
fieldCardsForTarget state =
  [ LocatedCard{locationOwner = owner, cardInPlay = card, location = Field}
  | owner <- [Player1, Player2]
  , card <- (playerById owner state).field
  ]

destroyLocatedCard :: HasStateIO r => CardInPlay -> Eff r ()
destroyLocatedCard card = do
  removedCards <- removeCards [card.id]
  addToGraveyard card.owner removedCards

sacrificeLocatedCard :: HasStateIO r => CardInPlay -> Eff r ()
sacrificeLocatedCard = destroyLocatedCard

returnCardToHand :: HasStateIO r => CardInPlay -> Eff r ()
returnCardToHand card = do
  removedCards <- removeCards [card.id]
  forM_ removedCards $ const $ addToHand card.owner card

takeLocatedCardToCurrentHand :: HasStateIO r => CardInPlay -> Eff r ()
takeLocatedCardToCurrentHand card = do
  state <- getGameState
  let triggeringPlayer = ownerOfTriggeringCard state card.id
  removed <- removeCards [card.id]
  forM_ removed $ addToHand triggeringPlayer

putNewCardOnField :: HasStateIO r => Player -> Card -> Eff r CardInPlay
putNewCardOnField owner card = do
  newCard <- createCardInPlay owner.playerId card
  addCardToField owner newCard

addCardToField :: HasStateIO r => Player -> CardInPlay -> Eff r CardInPlay
addCardToField owner cardInPlay = do
  let movedCard = cardInPlay{owner = owner.playerId}
  modifyPlayer owner.playerId \player -> player{field = player.field <> [movedCard]}
  pure movedCard

createCardInPlay :: State GameState :> r => PlayerId -> Card -> Eff r CardInPlay
createCardInPlay owner card = do
  state <- getGameState
  let cardInPlay = CardInPlay{id = CardId state.nextCardId, owner = owner, card = card, modifications = []}
  modify \current -> current{nextCardId = current.nextCardId + 1}
  pure cardInPlay

removeCards :: HasStateIO r => [CardId] -> Eff r [CardInPlay]
removeCards cardIds = do
  state <- getGameState
  let (player1, player2) = state.players
      (removedFromPlayer1, updatedPlayer1) = removeCardsFromPlayer cardIds player1
      (removedFromPlayer2, updatedPlayer2) = removeCardsFromPlayer cardIds player2
  modify \current -> current{players = (updatedPlayer1, updatedPlayer2)}
  pure $ removedFromPlayer1 <> removedFromPlayer2

removeCardsFromPlayer :: [CardId] -> Player -> ([CardInPlay], Player)
removeCardsFromPlayer cardIds player =
  let (removedFromField, remainingField) = partitionCards cardIds player.field
      (removedFromDeck, remainingDeck) = partitionCards cardIds player.deck
      (removedFromHand, remainingHand) = partitionCards cardIds player.hand
      (removedFromGraveyard, remainingGraveyard) = partitionCards cardIds player.graveyard
   in ( removedFromField <> removedFromDeck <> removedFromHand <> removedFromGraveyard
      , player
          { field = remainingField
          , deck = remainingDeck
          , hand = remainingHand
          , graveyard = remainingGraveyard
          }
      )

partitionCards :: [CardId] -> [CardInPlay] -> ([CardInPlay], [CardInPlay])
partitionCards cardIds = foldr step ([], [])
 where
  step cardInPlay (removed, kept)
    | cardInPlay.id `elem` cardIds = (cardInPlay : removed, kept)
    | otherwise = (removed, cardInPlay : kept)

removeFromHand :: HasStateIO r => Player -> Int -> Eff r (Maybe CardInPlay)
removeFromHand owner index =
  case removeAt index owner.hand of
    Nothing -> pure Nothing
    Just (card, remainingHand) -> do
      modifyPlayer owner.playerId \current -> current{hand = remainingHand}
      pure $ Just card

addToHand :: HasStateIO r => PlayerId -> CardInPlay -> Eff r ()
addToHand owner card = modifyPlayer owner \player -> player{hand = player.hand <> [card]}

addToGraveyard :: HasStateIO r => PlayerId -> [CardInPlay] -> Eff r ()
addToGraveyard owner cards = modifyPlayer owner \player -> player{graveyard = player.graveyard <> cards}

drawCardsForCurrentPlayer :: HasStateIO r => Int -> Eff r ()
drawCardsForCurrentPlayer n = do
  activePlayer <- gets currentPlayer
  replicateM_ n do
    player <- gets currentPlayer
    case player.deck of
      [] -> pure ()
      card : restDeck ->
        modifyPlayer activePlayer.playerId \current -> current{deck = restDeck, hand = current.hand <> [card]}

drawOpeningHands :: GameState -> GameState
drawOpeningHands = drawCardsPure Player2 5 . drawCardsPure Player1 5

drawCardsPure :: PlayerId -> Int -> GameState -> GameState
drawCardsPure owner n state =
  foldr (\_ current -> fromMaybe current $ drawOnePure owner current) state [1 .. n]

drawOnePure :: PlayerId -> GameState -> Maybe GameState
drawOnePure owner state = case (playerById owner state).deck of
  [] -> Nothing
  card : restDeck ->
    Just $
      modifyPlayerPure
        owner
        (\player -> player{deck = restDeck, hand = player.hand <> [card]})
        state

playerById :: PlayerId -> GameState -> Player
playerById owner state = case state.players of
  (player1, player2) -> case owner of
    Player1 -> player1
    Player2 -> player2

modifyPlayer :: HasStateIO r => PlayerId -> (Player -> Player) -> Eff r ()
modifyPlayer owner update = modify (modifyPlayerPure owner update)

modifyCurrentPlayer :: HasStateIO r => (Player -> Player) -> Eff r ()
modifyCurrentPlayer update = do
  activePlayer <- gets currentPlayer
  modifyPlayer activePlayer.playerId update

modifyPlayerPure :: PlayerId -> (Player -> Player) -> GameState -> GameState
modifyPlayerPure owner update state = case state.players of
  (player1, player2) -> case owner of
    Player1 -> state{players = (update player1, player2)}
    Player2 -> state{players = (player1, update player2)}

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt index values
  | index < 0 = Nothing
  | otherwise = case splitAt index values of
      (before, value : after) -> Just (value, before <> after)
      _ -> Nothing

atMay :: [a] -> Int -> Maybe a
atMay values index
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value : _ -> Just value
      [] -> Nothing

removeFirstById :: CardId -> [CardInPlay] -> [CardInPlay]
removeFirstById idToRemove = go
 where
  go [] = []
  go (card : rest)
    | card.id == idToRemove = rest
    | otherwise = card : go rest

matchesViewedCard :: Ziel -> CardInPlay -> Bool
matchesViewedCard ziel cardInPlay =
  viewedTypeMatches ziel.ziel.description cardInPlay.card
    && viewedCostMatches ziel.ziel.description cardInPlay.card

viewedTypeMatches :: String -> Card -> Bool
viewedTypeMatches desc card
  | "Gegenmagie" `isInfixOf` desc = case card.cardType of
      Gegenmagie -> True
      _ -> False
  | "Magie" `isInfixOf` desc = case card.cardType of
      Allmagie -> True
      Gegenmagie -> True
      Magie -> True
      MagieDauerhaft -> True
      _ -> False
  | "Wesen" `isInfixOf` desc = case card.cardType of
      Wesen _ _ -> True
      _ -> False
  | "Karte" `isInfixOf` desc || "Karten" `isInfixOf` desc = True
  | otherwise = True

viewedCostMatches :: String -> Card -> Bool
viewedCostMatches desc card = case words desc of
  ["mit", "kosten", "von", number, "oder", "weniger"] -> gesamtKosten card.cost <= read number
  _ -> True
