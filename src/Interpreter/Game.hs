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

import Control.Monad (forM_, replicateM_)
import Control.Monad.Free (Free (..), foldFree, iter, iterM)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe, maybeToList)
import DataTypes
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, gets)
import EffectfulLens (use, (%=), (++=), (+=), (-=), (.=))
import Element (gesamtKosten)
import GameEffects (ChoiceInput, Log)
import GameIO qualified as Gio
import GameState (currentPlayer, currentPlayerL, getGameState, opponentPlayer, opponentPlayerL, playerByIdL)
import Optics (AffineTraversal', (%), (^..))
import Optics.AffineTraversal (unsafeFiltered)
import Optics.Label ()
import Optics.Lens (Lens')
import Target (ein, ownerOfTriggeringCard, wesen)

type HasStateIO es = (State GameState :> es, ChoiceInput :> es, Log :> es)
type HasState es = (State GameState :> es)

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
        then addCardToField activePlayer card
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
  WähleZiel ziel effectForTarget next -> do
    targets <- selectTargets sourceId ziel
    case targets of
      [] -> pure ()
      _ -> runEffect sourceId (effectForTarget $ concreteTarget ziel targets)
    pure next
  Opfere ziel next -> do
    sacrificeTargets sourceId ziel
    pure next
  GegnerOpfert ziel next -> do
    opponentSacrifices sourceId ziel
    pure next
  Heile anzahl next -> do
    currentPlayerL % #schicksalsmacht += anzahlToInt anzahl
    pure next
  Schade anzahl next -> do
    damageOpponent (anzahlToInt anzahl)
    pure next
  ZerstöreSchwächeres zielA zielB next -> do
    destroyWeakerTarget sourceId zielA zielB
    pure next
  GibInsDeck woInsDeck ziel next -> do
    returnTargetsToDeck sourceId woInsDeck ziel
    pure next
  GibAufDieHandZurück ziel next -> do
    bounceTargets sourceId ziel
    pure next
  Zerstöre ziel next -> do
    destroyTargets sourceId ziel
    pure next
  EntferneAusDemSpiel ziel next -> do
    removeFromGame sourceId ziel
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
  GegnerWirfAb anzahl _ next -> do
    discardFromOpponentHand (anzahlToInt anzahl)
    pure next
  LegeVomDeckAufDenFriedhof anzahl _ next -> do
    millCurrentDeck (anzahlToInt anzahl)
    pure next
  SchaueObenVomDeck anzahl instructions next -> do
    inspectTopOfDeck sourceId (anzahlToInt anzahl) instructions
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

opponentSacrifices :: HasStateIO r => CardId -> Ziel -> Eff r ()
opponentSacrifices sourceId ziel = do
  state <- getGameState
  let opponent = opponentPlayer state
  targets <- selectableTargetsFromCards sourceId ziel.ziel opponent.field
  maybe (pure ()) sacrificeLocatedCard =<< chooseTargetFor opponent.playerId targets

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

removeFromGame :: HasStateIO r => CardId -> Ziel -> Eff r ()
removeFromGame sourceId ziel = do
  targets <- selectTargets sourceId ziel
  void $ removeCards $ fmap (.id) targets

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

concreteTarget :: Ziel -> [CardInPlay] -> Ziel
concreteTarget ziel selectedTargets =
  ziel
    { anzahl = Undefiniert
    , ziel =
        EinZiel
          { description = describeConcreteTargets selectedTargets
          , candidates = \_ _ _ -> selectedTargets
          }
    }

describeConcreteTargets :: [CardInPlay] -> String
describeConcreteTargets [] = "kein Ziel"
describeConcreteTargets [target] = target.card.name
describeConcreteTargets targets = intercalate ", " $ fmap (.card.name) targets

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
discardFromCurrentHand n = gets ((.playerId) . currentPlayer) >>= discardFromPlayerHand n

discardFromOpponentHand :: HasStateIO r => Int -> Eff r ()
discardFromOpponentHand n = gets ((.playerId) . opponentPlayer) >>= discardFromPlayerHand n

discardFromPlayerHand :: HasStateIO r => Int -> PlayerId -> Eff r ()
discardFromPlayerHand n playerId = replicateM_ n do
  player <- gets (playerById playerId)
  case player.hand of
    [] -> pure ()
    cards -> do
      choice <- Gio.chooseOne $ zip [1 ..] cards
      case choice of
        Nothing -> pure ()
        Just (pickedIndex, _) -> do
          maybeCard <- removeFromHand player (pickedIndex - 1)
          addToGraveyard player.playerId (maybeToList maybeCard)

millCurrentDeck :: HasStateIO r => Int -> Eff r ()
millCurrentDeck n = do
  deck <- use $ currentPlayerL % #deck
  let (milled, restDeck) = splitAt n deck
  currentPlayerL % #deck .= restDeck
  currentPlayerL % #graveyard ++= milled

inspectTopOfDeck :: HasStateIO r => CardId -> Int -> InstructionWhenViewingDeckF () -> Eff r ()
inspectTopOfDeck sourceId n instructions = do
  activePlayer <- gets currentPlayer
  let (viewedCards, restDeck) = splitAt n activePlayer.deck
  currentPlayerL % #deck .= restDeck
  remainingCards <- runViewedInstructions sourceId activePlayer.playerId viewedCards instructions
  currentPlayerL % #deck %= (remainingCards <>)

runViewedInstructions :: HasStateIO r => CardId -> PlayerId -> [CardInPlay] -> InstructionWhenViewingDeckF () -> Eff r [CardInPlay]
runViewedInstructions _ _ viewedCards (Pure ()) = pure viewedCards
runViewedInstructions sourceId playerId viewedCards (Free instruction) = case instruction of
  ZeigeVorUndNimmAufDieHand ziel next -> do
    remaining <- moveViewedCard sourceId viewedCards ziel \card -> playerByIdL playerId % #hand ++= [card]
    runViewedInstructions sourceId playerId remaining next
  ZeigeVorUndWirfAb ziel next -> do
    remaining <- moveViewedCard sourceId viewedCards ziel \card -> playerByIdL playerId % #graveyard ++= [card]
    runViewedInstructions sourceId playerId remaining next
  LegeRestUnterDasDeck next -> do
    playerByIdL playerId % #deck ++= viewedCards
    runViewedInstructions sourceId playerId [] next
  LegeRestAufDenFriedhof _ next -> do
    playerByIdL playerId % #graveyard ++= viewedCards
    runViewedInstructions sourceId playerId [] next
  WähleVomDeck options next -> do
    choice <- Gio.chooseOne [1 .. length options]
    afterChoice <- case choice of
      Nothing -> pure viewedCards
      Just picked -> runViewedInstructions sourceId playerId viewedCards (options !! (picked - 1))
    runViewedInstructions sourceId playerId afterChoice next

moveViewedCard :: HasStateIO r => CardId -> [CardInPlay] -> Ziel -> (CardInPlay -> Eff r ()) -> Eff r [CardInPlay]
moveViewedCard sourceId viewedCards ziel onMove = do
  options <- selectableTargetsFromCards sourceId ziel.ziel viewedCards
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
    BisZu anzahl ->
      chooseUpToTargetsFor triggeringPlayer (anzahlToInt anzahl) choices

chooseTargetFor :: (Log :> es, ChoiceInput :> es, Show a) => PlayerId -> [a] -> Eff es (Maybe a)
chooseTargetFor playerId choices = do
  Gio.logLn' $ show playerId <> " needs to choose."
  Gio.chooseOne choices

data TargetChoice a = Fertig | TargetOption a

instance Show a => Show (TargetChoice a) where
  show Fertig = "Fertig"
  show (TargetOption choice) = show choice

chooseUpToTargetsFor :: (Log :> es, ChoiceInput :> es, Show a, Eq a) => PlayerId -> Int -> [a] -> Eff es [a]
chooseUpToTargetsFor _ limit _ | limit <= 0 = pure []
chooseUpToTargetsFor playerId limit choices = go limit choices []
 where
  go _ [] selected = pure $ reverse selected
  go remaining available selected = do
    Gio.logLn' $ show playerId <> " may choose up to " <> show limit <> " targets."
    choice <- Gio.chooseOne (Fertig : fmap TargetOption available)
    case choice of
      Nothing -> pure $ reverse selected
      Just Fertig -> pure $ reverse selected
      Just (TargetOption picked) ->
        go (remaining - 1) (filter (/= picked) available) (picked : selected)

selectableTargets :: HasStateIO r => CardId -> EinZiel -> Eff r [CardInPlay]
selectableTargets sourceId ziel =
  selectableTargetsFromCards sourceId ziel =<< gets (^.. allCards)

selectableTargetsFromCards :: HasStateIO r => CardId -> EinZiel -> [CardInPlay] -> Eff r [CardInPlay]
selectableTargetsFromCards sourceId ziel availableCards = do
  state <- getGameState
  pure $ ziel.candidates state sourceId availableCards

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
    sequence_ [trigger | FähigkeitsModifikation _ trigger <- cardInPlay.modifications]

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

putNewCardOnField :: HasStateIO r => Player -> Card -> Eff r ()
putNewCardOnField owner card = do
  newCard <- createCardInPlay owner.playerId card
  addCardToField owner newCard

addCardToField :: HasStateIO r => Player -> CardInPlay -> Eff r ()
addCardToField owner card = playerByIdL owner.playerId % #field ++= [card]

createCardInPlay :: State GameState :> r => PlayerId -> Card -> Eff r CardInPlay
createCardInPlay owner card = do
  state <- getGameState
  -- TODO: instead implement a `cardId <- getNextCardId` function that auto increments the id
  let cardInPlay = CardInPlay{id = CardId state.nextCardId, owner, card, modifications = []}
  stateAt #nextCardId += 1
  pure cardInPlay

removeCards :: HasStateIO r => [CardId] -> Eff r [CardInPlay]
removeCards cardIds = do
  state <- getGameState
  let (player1, player2) = state.players
      (removedFromPlayer1, updatedPlayer1) = removeCardsFromPlayer cardIds player1
      (removedFromPlayer2, updatedPlayer2) = removeCardsFromPlayer cardIds player2
  stateAt #players .= (updatedPlayer1, updatedPlayer2)
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
      playerByIdL owner.playerId % #hand .= remainingHand
      pure $ Just card

addToHand :: HasStateIO r => PlayerId -> CardInPlay -> Eff r ()
addToHand owner card = playerByIdL owner % #hand ++= [card]

addToGraveyard :: HasStateIO r => PlayerId -> [CardInPlay] -> Eff r ()
addToGraveyard owner cards = playerByIdL owner % #graveyard ++= cards

damageOpponent :: HasStateIO r => Int -> Eff r ()
damageOpponent amount = opponentPlayerL % #schicksalsmacht -= amount

returnTargetsToDeck :: HasStateIO r => CardId -> WoInsDeck -> Ziel -> Eff r ()
returnTargetsToDeck sourceId woInsDeck ziel = do
  targets <- selectTargets sourceId ziel
  removedCards <- removeCards $ fmap (.id) targets
  forM_ removedCards \removedCard ->
    case woInsDeck of
      Oben -> playerByIdL removedCard.owner % #deck %= (removedCard :)
      Unten -> playerByIdL removedCard.owner % #deck ++= [removedCard]
      AnPosition position -> playerByIdL removedCard.owner % #deck %= insertAt (max 0 (position - 1)) removedCard

destroyWeakerTarget :: HasStateIO r => CardId -> Ziel -> Ziel -> Eff r ()
destroyWeakerTarget sourceId zielA zielB = do
  targetsA <- selectTargets sourceId zielA
  targetsB <- selectTargets sourceId zielB
  forM_
    (targetsA <> targetsB & sortOn creatureStrength & init)
    destroyLocatedCard

drawCardsForCurrentPlayer :: HasState r => Int -> Eff r ()
drawCardsForCurrentPlayer n = gets ((.playerId) . currentPlayer) >>= drawCardsForPlayer n

drawCardsForPlayer :: HasState r => Int -> PlayerId -> Eff r ()
drawCardsForPlayer numberOfCards playerId = replicateM_ numberOfCards do
  let player = playerByIdL playerId
  deck <- use $ player % #deck
  forM_ (listToMaybe deck) \card -> do
    player % #deck %= drop 1
    player % #hand ++= [card]

drawOpeningHands :: HasState r => Eff r ()
drawOpeningHands = do
  drawCardsForPlayer 5 Player1
  drawCardsForPlayer 5 Player2

playerById :: PlayerId -> GameState -> Player
playerById owner state = case state.players of
  (player1, player2) -> case owner of
    Player1 -> player1
    Player2 -> player2

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

insertAt :: Int -> a -> [a] -> [a]
insertAt index value values =
  let (before, after) = splitAt index values
   in before <> [value] <> after

-- to help with type inference on naked fields like `#nextCardId`
stateAt :: Lens' GameState b -> Lens' GameState b
stateAt = id
