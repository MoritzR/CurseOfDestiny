{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game where

import Cards (series26)
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Free (Free (..), iterM)
import Data.List (find, isInfixOf)
import Data.Maybe (maybeToList)
import DataTypes
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.State.Static.Local (State, evalState, execState, get, gets, modify)
import Element (gesamtKosten)
import GameActionParser (GameAction (..), parseGameAction)
import GameEffects (ChoiceInput, CommandInput, Log, ignoreLog, readCommand, runChoiceInputConst)
import GameIO qualified as Gio
import Target (ein, wesen)
import Prelude hiding (log)

data LocatedCard
  = FieldCard CardInPlay
  | HandCard PlayerId Int Card
  | GraveyardCard PlayerId Int Card
  deriving (Eq, Show)

createPlayer :: PlayerId -> String -> Player
createPlayer pid playerName =
  Player
    { name = playerName
    , playerId = pid
    , schicksalswesen = PlatzhalterSchicksalswesen
    , deck = series26
    , hand = []
    , field = []
    , graveyard = []
    , schicksalsmacht = 0
    }

initialGameState :: GameState
initialGameState =
  drawOpeningHands $
    GameState
      { players = (createPlayer Player1 "player1", createPlayer Player2 "player2")
      , nextCardId = 1
      }

playGame :: HasStateIO r => [GameAction] -> Eff r ()
playGame = mapM_ resolveAction

runGameActions :: Int -> GameState -> [GameAction] -> IO GameState
runGameActions firstChoice gameState actions =
  runEff $
    execState gameState $
      runChoiceInputConst firstChoice $
        ignoreLog $
          playGame actions

creatureStrength :: CardInPlay -> Int
creatureStrength cardInPlay = baseStrength cardInPlay.card + sum (strengthDelta <$> cardInPlay.modifications)

gameOver :: Log :> es => Eff es ()
gameOver = Gio.logLn' "k bye"

type Game es a = HasStateIO es => Eff es a
type HasStateIO es = (State GameState :> es, ChoiceInput :> es, Log :> es)

currentPlayer :: GameState -> Player
currentPlayer = fst . (.players)

opponentPlayer :: GameState -> Player
opponentPlayer = snd . (.players)

currentPlayerId :: GameState -> PlayerId
currentPlayerId = (.playerId) . currentPlayer

gameLoop :: (CommandInput :> es, HasStateIO es) => Eff es ()
gameLoop = do
  activePlayer <- gets currentPlayer
  opponent <- gets opponentPlayer

  Gio.logLn' $ "Am Zug: " <> activePlayer.name
  Gio.logLn' $ "Gegnerische Schicksalsmacht: " <> show opponent.schicksalsmacht
  Gio.logLn' "Enemy field:"
  logField opponent

  Gio.logLn' $ "Deine Schicksalsmacht: " <> show activePlayer.schicksalsmacht
  Gio.logLn' "Your field:"
  logField activePlayer

  Gio.logLn' "Player Hand:"
  logHand activePlayer

  Gio.log' "Select action (pass/end/p/c/a/d): "
  inp <- readCommand
  if inp == "exit" || inp == "q"
    then gameOver
    else do
      case parseGameAction inp of
        Nothing -> Gio.logLn' "Ungültige Eingabe."
        Just action -> playGame [action]
      gameLoop

startGame :: (IOE :> es, CommandInput :> es, ChoiceInput :> es, Log :> es) => Eff es ()
startGame = evalState initialGameState gameLoop

resolveAction :: HasStateIO r => GameAction -> Eff r ()
resolveAction = \case
  PlayFromHand index -> playCardFromHand index
  ActivateFromField index -> activateCardOnField index
  AnnounceAttack source target ->
    Gio.logLn' $ "Angriff ist noch nicht implementiert: " <> show (source + 1) <> " -> " <> show (target + 1)
  AnnounceDirectAttack source ->
    Gio.logLn' $ "Direkter Angriff ist noch nicht implementiert: " <> show (source + 1)
  Pass ->
    Gio.logLn' "Passe."
  EndRound ->
    endRound
  Play card ->
    Gio.logLn' $ "Direktes Spielen ist nicht implementiert: " <> card.name

playCardFromHand :: HasStateIO r => Int -> Eff r ()
playCardFromHand index = do
  activePlayer <- gets currentPlayerId
  maybeCard <- removeFromHand activePlayer index
  case maybeCard of
    Nothing ->
      Gio.logLn' "Keine Karte auf diesem Hand-Slot."
    Just card -> do
      maybeSource <- case card.cardType of
        Wesen _ _ -> Just <$> putCardOnField activePlayer card
        MagieDauerhaft -> Just <$> putCardOnField activePlayer card
        _ -> pure Nothing
      forM_ maybeSource do
        runOnPlayTrigger card.trigger
      case card.cardType of
        Allmagie -> addToGraveyard activePlayer card
        Magie -> addToGraveyard activePlayer card
        Gegenmagie -> addToGraveyard activePlayer card
        _ -> pure ()

activateCardOnField :: HasStateIO r => Int -> Eff r ()
activateCardOnField index = do
  activePlayer <- gets currentPlayerId
  field <- gets \state -> (playerById activePlayer state).field
  case atMay field index of
    Nothing ->
      Gio.logLn' "Keine Karte auf diesem Feld-Slot."
    Just source -> do
      let activations = collectActivations source.card.trigger
      case activations of
        [] ->
          Gio.logLn' "Diese Karte hat keine aktivierbaren Effekte."
        [activation] ->
          runEffect (Just source.id) activation
        _ -> do
          Gio.logLn' "Waehle einen Effekt:"
          choice <- Gio.chooseOne [1 .. length activations]
          maybe (pure ()) (\picked -> runEffect (Just source.id) (activations !! (picked - 1))) choice

endRound :: HasStateIO r => Eff r ()
endRound = do
  modify \state ->
    state
      { players =
          let (active, opponent) = state.players
           in ( opponent{field = fmap removeTemporaryModifications opponent.field}
              , active{field = fmap removeTemporaryModifications active.field}
              )
      }
  Gio.logLn' "Runde beendet."

runOnPlayTrigger :: HasStateIO r => Trigger -> CardInPlay -> Eff r ()
runOnPlayTrigger trigger source =
  iterM (\instruction -> runPlayTriggerInstruction source instruction *> sequence_ instruction) trigger

runPlayTriggerInstruction :: HasStateIO r => CardInPlay -> TriggerInstruction (Eff r ()) -> Eff r ()
runPlayTriggerInstruction source = \case
  WennGespielt effect _ ->
    runEffect (Just source.id) effect
  _ ->
    pure ()

collectActivations :: Trigger -> [CardEffect]
collectActivations = \case
  Pure () -> []
  Free instruction -> case instruction of
    Zahle _ effect next -> effect : collectActivations next
    EinmalProRunde effect next -> effect : collectActivations next
    AmEndeDerRunde _ next -> collectActivations next
    AmBeginnDerRunde _ next -> collectActivations next
    WennGespielt _ next -> collectActivations next
    WennAufDemFeld _ next -> collectActivations next
    BeimAngriff _ _ next -> collectActivations next
    Blockierung next -> collectActivations next
    Doppelzerstörung next -> collectActivations next
    Lebensentzug next -> collectActivations next
    KannNichtAbwehren next -> collectActivations next

runEffect :: HasStateIO r => Maybe String -> CardEffect -> Eff r ()
runEffect maybeSourceId = \case
  Pure () -> pure ()
  Free instruction -> case instruction of
    Ziehe anzahl next -> do
      drawCardsForCurrentPlayer (anzahlToInt anzahl)
      runEffect maybeSourceId next
    Erhöhe wert ziel dauer höhe next -> do
      increaseValue maybeSourceId wert ziel dauer (anzahlToInt höhe)
      runEffect maybeSourceId next
    Vision _ next ->
      runEffect maybeSourceId next
    Prisma effectForX next -> do
      runEffect maybeSourceId (effectForX 0)
      runEffect maybeSourceId next
    Spende _ _ next ->
      runEffect maybeSourceId next
    WähleAus options effectForOption next -> do
      choice <- Gio.chooseOne options
      maybe (pure ()) (runEffect maybeSourceId . effectForOption) choice
      runEffect maybeSourceId next
    WähleEffekt effects next -> do
      choice <- Gio.chooseOne [1 .. length effects]
      maybe (pure ()) (\picked -> runEffect maybeSourceId (effects !! (picked - 1))) choice
      runEffect maybeSourceId next
    Opfere ziel next -> do
      sacrificeTargets maybeSourceId ziel
      runEffect maybeSourceId next
    Heile anzahl next -> do
      modifyCurrentPlayer \player -> player{schicksalsmacht = player.schicksalsmacht + anzahlToInt anzahl}
      runEffect maybeSourceId next
    GibAufDieHandZurück ziel next -> do
      bounceTargets maybeSourceId ziel
      runEffect maybeSourceId next
    Zerstöre ziel next -> do
      destroyTargets maybeSourceId ziel
      runEffect maybeSourceId next
    Verringere wert ziel dauer höhe next -> do
      increaseValue maybeSourceId wert ziel dauer (negate $ anzahlToInt höhe)
      runEffect maybeSourceId next
    VerringereUndZerstöre ziel dauer höhe next -> do
      increaseValue maybeSourceId Stärke ziel dauer (negate $ anzahlToInt höhe)
      destroyDeadCreatures
      runEffect maybeSourceId next
    NimmAufDieHand ziel next -> do
      takeTargetsToHand maybeSourceId ziel
      runEffect maybeSourceId next
    ZeigeObenVomDeck anzahl lesbarerWert effectForX next -> do
      value <- readTopOfDeckValue (anzahlToInt anzahl) lesbarerWert
      runEffect maybeSourceId (effectForX value)
      runEffect maybeSourceId next
    BringeInsSpiel card next -> do
      activePlayer <- gets currentPlayerId
      _ <- putCardOnField activePlayer card
      runEffect maybeSourceId next
    BringeInsSpielAusZiel ziel next -> do
      bringTargetIntoPlay maybeSourceId ziel
      runEffect maybeSourceId next
    GibFähigkeit ziel dauer _ next -> do
      addAbilityToTargets maybeSourceId ziel dauer
      runEffect maybeSourceId next
    EinSpielerOpfertEinWesen next -> do
      sacrificeTargets maybeSourceId (ein wesen)
      runEffect maybeSourceId next
    AnzahlVon ziel effectForAmount next -> do
      amount <- countTargets maybeSourceId ziel
      runEffect maybeSourceId (effectForAmount amount)
      runEffect maybeSourceId next
    WirfAb anzahl _ next -> do
      discardFromCurrentHand (anzahlToInt anzahl)
      runEffect maybeSourceId next
    LegeVomDeckAufDenFriedhof anzahl _ next -> do
      millCurrentDeck (anzahlToInt anzahl)
      runEffect maybeSourceId next
    SchaueObenVomDeck anzahl instructions next -> do
      inspectTopOfDeck (anzahlToInt anzahl) instructions
      runEffect maybeSourceId next
    SiehHandkartenAnUndEntferneEineAusDemSpiel next ->
      runEffect maybeSourceId next
    BringeKopieInsSpiel ziel next -> do
      copyTargetIntoPlay maybeSourceId ziel
      runEffect maybeSourceId next
    AnzahlSchicksalsMächte spielerZiel effectForAmount next -> do
      amount <- readSchicksalsmächte spielerZiel
      runEffect maybeSourceId (effectForAmount amount)
      runEffect maybeSourceId next

increaseValue :: HasStateIO r => Maybe String -> Wert -> Ziel -> Dauer -> Int -> Eff r ()
increaseValue maybeSourceId Stärke ziel dauer delta = do
  targets <- selectTargets maybeSourceId ziel
  forM_ targets \case
    FieldCard fieldCard ->
      modifyFieldCard fieldCard.id \cardInPlay ->
        cardInPlay{modifications = cardInPlay.modifications <> [StärkeModifikation dauer delta]}
    _ ->
      pure ()

sacrificeTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
sacrificeTargets maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  mapM_ sacrificeLocatedCard targets

destroyTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
destroyTargets maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  mapM_ destroyLocatedCard targets

bounceTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
bounceTargets maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  mapM_ returnLocatedCardToHand targets

takeTargetsToHand :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
takeTargetsToHand maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  mapM_ takeLocatedCardToCurrentHand targets

bringTargetIntoPlay :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
bringTargetIntoPlay maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  case targets of
    [] -> pure ()
    (target : _) -> do
      activePlayer <- gets currentPlayerId
      maybeCard <- removeLocatedCard target
      maybe (pure ()) (void . putCardOnField activePlayer) maybeCard

copyTargetIntoPlay :: HasStateIO r => Maybe String -> Ziel -> Eff r ()
copyTargetIntoPlay maybeSourceId ziel = do
  targets <- selectTargets maybeSourceId ziel
  case targets of
    FieldCard fieldCard : _ -> do
      activePlayer <- gets currentPlayerId
      void $ putCardOnField activePlayer fieldCard.card
    _ -> pure ()

addAbilityToTargets :: HasStateIO r => Maybe String -> Ziel -> Dauer -> Eff r ()
addAbilityToTargets maybeSourceId ziel dauer = do
  targets <- selectTargets maybeSourceId ziel
  forM_ targets \case
    FieldCard fieldCard ->
      modifyFieldCard fieldCard.id \cardInPlay ->
        cardInPlay{modifications = cardInPlay.modifications <> [FähigkeitsModifikation dauer]}
    _ ->
      pure ()

countTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r Anzahl
countTargets maybeSourceId ziel = Actual . length <$> selectableTargets maybeSourceId ziel

readTopOfDeckValue :: HasStateIO r => Int -> LesbarerWert -> Eff r Anzahl
readTopOfDeckValue n LesbarKosten = do
  activePlayer <- gets currentPlayerId
  player <- gets (playerById activePlayer)
  let topCards = take n player.deck
  pure $ Actual $ sum (gesamtKosten . (.cost) <$> topCards)

readSchicksalsmächte :: HasStateIO r => SpielerZiel -> Eff r Anzahl
readSchicksalsmächte spielerZiel = do
  activePlayer <- gets currentPlayerId
  let targetPlayer = case spielerZiel of
        Du -> activePlayer
        Gegner -> otherPlayer activePlayer
  Actual . (.schicksalsmacht) <$> gets (playerById targetPlayer)

discardFromCurrentHand :: HasStateIO r => Int -> Eff r ()
discardFromCurrentHand n = replicateM_ n do
  activePlayer <- gets currentPlayerId
  player <- gets (playerById activePlayer)
  case player.hand of
    [] -> pure ()
    cards -> do
      choice <- Gio.chooseOne $ zip [1 ..] cards
      case choice of
        Nothing -> pure ()
        Just (pickedIndex, _) -> do
          maybeCard <- removeFromHand activePlayer (pickedIndex - 1)
          maybe (pure ()) (addToGraveyard activePlayer) maybeCard

millCurrentDeck :: HasStateIO r => Int -> Eff r ()
millCurrentDeck n = do
  activePlayer <- gets currentPlayerId
  player <- gets (playerById activePlayer)
  let (milled, restDeck) = splitAt n player.deck
  modifyPlayer activePlayer \current ->
    current{deck = restDeck, graveyard = current.graveyard <> milled}

inspectTopOfDeck :: HasStateIO r => Int -> InstructionWhenViewingDeckF () -> Eff r ()
inspectTopOfDeck n instructions = do
  activePlayer <- gets currentPlayerId
  player <- gets (playerById activePlayer)
  let (viewedCards, restDeck) = splitAt n player.deck
  modifyPlayer activePlayer \current -> current{deck = restDeck}
  remainingCards <- runViewedInstructions activePlayer viewedCards instructions
  modifyPlayer activePlayer \current -> current{deck = remainingCards <> current.deck}

runViewedInstructions :: HasStateIO r => PlayerId -> [Card] -> InstructionWhenViewingDeckF () -> Eff r [Card]
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

moveViewedCard :: HasStateIO r => [Card] -> Ziel -> (Card -> Eff r ()) -> Eff r [Card]
moveViewedCard viewedCards ziel onMove = do
  let options = filter (matchesViewedCard ziel) viewedCards
  case options of
    [] -> pure viewedCards
    [singleCard] -> do
      onMove singleCard
      pure $ removeFirstByName singleCard.name viewedCards
    _ -> do
      choice <- Gio.chooseOne options
      case choice of
        Nothing -> pure viewedCards
        Just card -> do
          onMove card
          pure $ removeFirstByName card.name viewedCards

selectTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r [LocatedCard]
selectTargets maybeSourceId ziel = do
  choices <- selectableTargets maybeSourceId ziel
  case ziel.anzahl of
    Alle -> pure choices
    _ -> do
      case choices of
        [] -> pure []
        [singleChoice] -> pure [singleChoice]
        _ -> maybeToList <$> Gio.chooseOne choices

selectableTargets :: HasStateIO r => Maybe String -> Ziel -> Eff r [LocatedCard]
selectableTargets maybeSourceId ziel = do
  state <- get
  let activePlayer = currentPlayerId state
      desc = ziel.ziel.description
      candidates = case () of
        _
          | desc == "diese Karte" ->
              maybe [] (\sourceId -> maybeToList $ findFieldCardById sourceId state) maybeSourceId
          | "auf dem Friedhof" `isInfixOf` desc ->
              graveyardCardsForTarget activePlayer state
          | "auf der Hand" `isInfixOf` desc ->
              handCardsForTarget activePlayer state
          | otherwise ->
              fieldCardsForTarget state
  pure $ filter (matchesTarget activePlayer desc) candidates

matchesTarget :: PlayerId -> String -> LocatedCard -> Bool
matchesTarget activePlayer desc locatedCard =
  ownerMatches activePlayer desc locatedCard
    && typeMatches desc (locatedCardCard locatedCard)
    && costMatches desc (locatedCardCard locatedCard)

ownerMatches :: PlayerId -> String -> LocatedCard -> Bool
ownerMatches activePlayer desc locatedCard
  | any (`isInfixOf` desc) ["eigene", "eigenes"] = locatedCardOwner locatedCard == activePlayer
  | "gegnerisches" `isInfixOf` desc = locatedCardOwner locatedCard == otherPlayer activePlayer
  | otherwise = True

typeMatches :: String -> Card -> Bool
typeMatches desc card
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

costMatches :: String -> Card -> Bool
costMatches desc card = case parseMaxCost desc of
  Nothing -> True
  Just maxCost -> gesamtKosten card.cost <= maxCost

parseMaxCost :: String -> Maybe Int
parseMaxCost desc =
  case words desc of
    ["mit", "kosten", "von", number, "oder", "weniger"] -> readMaybeInt number
    _ -> Nothing

removeTemporaryModifications :: CardInPlay -> CardInPlay
removeTemporaryModifications cardInPlay =
  cardInPlay{modifications = filter isPermanent cardInPlay.modifications}
 where
  isPermanent = \case
    StärkeModifikation Dauerhaft _ -> True
    FähigkeitsModifikation Dauerhaft -> True
    _ -> False

destroyDeadCreatures :: HasStateIO r => Eff r ()
destroyDeadCreatures = do
  deadCards <- filter isDeadCreature <$> gets allFieldCards
  mapM_ destroyLocatedCard (FieldCard <$> deadCards)

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
  FähigkeitsModifikation _ -> 0

locatedCardOwner :: LocatedCard -> PlayerId
locatedCardOwner = \case
  FieldCard cardInPlay -> cardInPlay.owner
  HandCard owner _ _ -> owner
  GraveyardCard owner _ _ -> owner

locatedCardCard :: LocatedCard -> Card
locatedCardCard = \case
  FieldCard cardInPlay -> cardInPlay.card
  HandCard _ _ card -> card
  GraveyardCard _ _ card -> card

fieldCardsForTarget :: GameState -> [LocatedCard]
fieldCardsForTarget state = FieldCard <$> allFieldCards state

handCardsForTarget :: PlayerId -> GameState -> [LocatedCard]
handCardsForTarget activePlayer state =
  [ HandCard owner index card
  | owner <- [activePlayer, otherPlayer activePlayer]
  , let cards = (playerById owner state).hand
  , (index, card) <- zip [0 ..] cards
  ]

graveyardCardsForTarget :: PlayerId -> GameState -> [LocatedCard]
graveyardCardsForTarget activePlayer state =
  [ GraveyardCard owner index card
  | owner <- [activePlayer, otherPlayer activePlayer]
  , let cards = (playerById owner state).graveyard
  , (index, card) <- zip [0 ..] cards
  ]

destroyLocatedCard :: HasStateIO r => LocatedCard -> Eff r ()
destroyLocatedCard = \case
  FieldCard cardInPlay -> do
    removed <- removeFieldCard cardInPlay.id
    maybe (pure ()) (\removedCard -> addToGraveyard removedCard.owner removedCard.card) removed
  HandCard owner index _ -> do
    removed <- removeFromHand owner index
    maybe (pure ()) (addToGraveyard owner) removed
  GraveyardCard _ _ _ ->
    pure ()

sacrificeLocatedCard :: HasStateIO r => LocatedCard -> Eff r ()
sacrificeLocatedCard = destroyLocatedCard

returnLocatedCardToHand :: HasStateIO r => LocatedCard -> Eff r ()
returnLocatedCardToHand = \case
  FieldCard cardInPlay -> do
    removed <- removeFieldCard cardInPlay.id
    maybe (pure ()) (\removedCard -> addToHand removedCard.owner removedCard.card) removed
  HandCard _ _ _ -> pure ()
  GraveyardCard owner index _ -> do
    removed <- removeFromGraveyard owner index
    maybe (pure ()) (addToHand owner) removed

takeLocatedCardToCurrentHand :: HasStateIO r => LocatedCard -> Eff r ()
takeLocatedCardToCurrentHand locatedCard = do
  activePlayer <- gets currentPlayerId
  removed <- removeLocatedCard locatedCard
  maybe (pure ()) (addToHand activePlayer) removed

removeLocatedCard :: HasStateIO r => LocatedCard -> Eff r (Maybe Card)
removeLocatedCard = \case
  FieldCard cardInPlay -> fmap (fmap (.card)) (removeFieldCard cardInPlay.id)
  HandCard owner index _ -> removeFromHand owner index
  GraveyardCard owner index _ -> removeFromGraveyard owner index

putCardOnField :: HasStateIO r => PlayerId -> Card -> Eff r CardInPlay
putCardOnField owner card = do
  (state :: GameState) <- get
  let cardId = show state.nextCardId
      cardInPlay = CardInPlay{id = cardId, owner = owner, card = card, modifications = []}
  modify \current -> modifyPlayerPure owner (\player -> player{field = player.field <> [cardInPlay]}) current{nextCardId = current.nextCardId + 1}
  pure cardInPlay

modifyFieldCard :: HasStateIO r => String -> (CardInPlay -> CardInPlay) -> Eff r ()
modifyFieldCard cardId update =
  modify \state ->
    modifyAllFields (\cardInPlay -> if cardInPlay.id == cardId then update cardInPlay else cardInPlay) state

removeFieldCard :: HasStateIO r => String -> Eff r (Maybe CardInPlay)
removeFieldCard cardId = do
  state <- get
  let maybeCard = findRawFieldCardById cardId state
  modify \current ->
    modifyPlayersPure
      (\player -> player{field = filter (\cardInPlay -> cardInPlay.id /= cardId) player.field})
      current
  pure maybeCard

removeFromHand :: HasStateIO r => PlayerId -> Int -> Eff r (Maybe Card)
removeFromHand owner index = do
  player <- gets (playerById owner)
  case removeAt index player.hand of
    Nothing -> pure Nothing
    Just (card, remainingHand) -> do
      modifyPlayer owner \current -> current{hand = remainingHand}
      pure $ Just card

removeFromGraveyard :: HasStateIO r => PlayerId -> Int -> Eff r (Maybe Card)
removeFromGraveyard owner index = do
  player <- gets (playerById owner)
  case removeAt index player.graveyard of
    Nothing -> pure Nothing
    Just (card, remainingGraveyard) -> do
      modifyPlayer owner \current -> current{graveyard = remainingGraveyard}
      pure $ Just card

addToHand :: HasStateIO r => PlayerId -> Card -> Eff r ()
addToHand owner card = modifyPlayer owner \player -> player{hand = player.hand <> [card]}

addToGraveyard :: HasStateIO r => PlayerId -> Card -> Eff r ()
addToGraveyard owner card = modifyPlayer owner \player -> player{graveyard = player.graveyard <> [card]}

drawCardsForCurrentPlayer :: HasStateIO r => Int -> Eff r ()
drawCardsForCurrentPlayer n = do
  activePlayer <- gets currentPlayerId
  replicateM_ n do
    player <- gets (playerById activePlayer)
    case player.deck of
      [] -> pure ()
      card : restDeck ->
        modifyPlayer activePlayer \current -> current{deck = restDeck, hand = current.hand <> [card]}

drawOpeningHands :: GameState -> GameState
drawOpeningHands = drawCardsPure Player2 5 . drawCardsPure Player1 5

drawCardsPure :: PlayerId -> Int -> GameState -> GameState
drawCardsPure owner n state =
  foldr (\_ current -> maybe current id $ drawOnePure owner current) state [1 .. n]

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

modifyPlayersPure :: (Player -> Player) -> GameState -> GameState
modifyPlayersPure update state = case state.players of
  (player1, player2) -> state{players = (update player1, update player2)}

modifyPlayer :: HasStateIO r => PlayerId -> (Player -> Player) -> Eff r ()
modifyPlayer owner update = modify (modifyPlayerPure owner update)

modifyCurrentPlayer :: HasStateIO r => (Player -> Player) -> Eff r ()
modifyCurrentPlayer update = do
  activePlayer <- gets currentPlayerId
  modifyPlayer activePlayer update

modifyPlayerPure :: PlayerId -> (Player -> Player) -> GameState -> GameState
modifyPlayerPure owner update state = case state.players of
  (player1, player2) -> case owner of
    Player1 -> state{players = (update player1, player2)}
    Player2 -> state{players = (player1, update player2)}

otherPlayer :: PlayerId -> PlayerId
otherPlayer = \case
  Player1 -> Player2
  Player2 -> Player1

allFieldCards :: GameState -> [CardInPlay]
allFieldCards state =
  let (player1, player2) = state.players
   in player1.field <> player2.field

modifyAllFields :: (CardInPlay -> CardInPlay) -> GameState -> GameState
modifyAllFields update =
  modifyPlayersPure \player -> player{field = fmap update player.field}

findRawFieldCardById :: String -> GameState -> Maybe CardInPlay
findRawFieldCardById cardId state = find (\cardInPlay -> cardInPlay.id == cardId) (allFieldCards state)

findFieldCardById :: String -> GameState -> Maybe LocatedCard
findFieldCardById cardId state = FieldCard <$> findRawFieldCardById cardId state

cardsForPlayer :: PlayerId -> GameState -> [CardInPlay]
cardsForPlayer owner state = (playerById owner state).field

logField :: HasStateIO r => Player -> Eff r ()
logField owner = do
  if null owner.field
    then Gio.logLn' "  (empty)"
    else Gio.displayEnumeratedItems $ renderFieldCard <$> owner.field

logHand :: HasStateIO r => Player -> Eff r ()
logHand player =
  if null player.hand
    then Gio.logLn' "  (empty)"
    else Gio.displayEnumeratedItems player.hand

renderFieldCard :: CardInPlay -> String
renderFieldCard cardInPlay = case cardInPlay.card.cardType of
  Wesen _ _ ->
    cardInPlay.card.name <> " [" <> show (creatureStrength cardInPlay) <> "]"
  _ ->
    cardInPlay.card.name

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

readMaybeInt :: String -> Maybe Int
readMaybeInt value = case reads value of
  [(number, "")] -> Just number
  _ -> Nothing

removeFirstByName :: String -> [Card] -> [Card]
removeFirstByName nameToRemove = go
 where
  go [] = []
  go (card : rest)
    | card.name == nameToRemove = rest
    | otherwise = card : go rest

matchesViewedCard :: Ziel -> Card -> Bool
matchesViewedCard ziel = typeMatches ziel.ziel.description
