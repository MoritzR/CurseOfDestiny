{-# LANGUAGE LambdaCase #-}

module Actions (resolve, deleteFirst, creaturePower, modifiedField) where

import Control.Lens (over, (%~), (^.), (^..))
import Control.Lens.Setter (set)
import Data.Function ((&))
import Data.Generics.Sum.Constructors (_Ctor')
import Data.Generics.Sum.Subtype (_Sub)
import Data.Tuple (swap)
import DataTypes
import DataTypes (GameState (GameState))
import qualified GameIO as Gio
import qualified Polysemy.State as S
import PolysemyLens ((%=), (++=), (-=))

resolve :: Action -> Game r ()
resolve action = case action of
  AddToField c -> addToField c
  Choose l -> resolveChoose l
  Destroy cardLens c -> destroy cardLens c
  Attack target source -> attack target source
  DirectAttack _source targetPlayerLens -> directAttack _source targetPlayerLens
  DiscardFromHand c -> discardFromHand c
  DestroyOne cardLens -> destroyOne cardLens
  Draw playerLens -> draw playerLens
  EndTurn -> endRound

endRound :: Game r ()
endRound = do
  applyTurnEnds
  S.modify changeCurrentPlayer

changeCurrentPlayer :: GameState -> GameState
changeCurrentPlayer = over #players swap

applyTurnEnds :: Game r ()
applyTurnEnds = do
  gs <- S.get
  let actions = gs ^. activePlayer . #field . traverse . #effects . #onTurnEnd
  mapM_ resolve actions

pass :: GameState -> GameState
pass = id

creaturePower :: Card -> Int
creaturePower card = case card ^. #cardType of
  Creature power -> power
  Spell -> 0 -- is a default a good idea here, or should this fail instead?

attack :: Card -> Card -> Game r ()
attack target source = do
  let (targetPower, sourcePower) = (creaturePower target, creaturePower source)

  case compare targetPower sourcePower of
    LT -> destroy (enemyPlayer . #field) target
    GT -> destroy (activePlayer . #field) source
    EQ -> do
      destroy (enemyPlayer . #field) target
      destroy (activePlayer . #field) source

addToField :: Card -> Game r ()
addToField card = do
  activePlayer . #field ++= [card]

destroy :: CardLens -> Card -> Game r ()
destroy cardLens card = do
  cardLens %= deleteFirst card

directAttack :: Card -> PlayerLens -> Game r ()
directAttack _source targetPlayer = do
  targetPlayer . playerHp -= 1

discardFromHand :: Card -> Game r ()
discardFromHand card = do
  activePlayer . #hand %= deleteFirst card

destroyOne :: CardLens -> Game r ()
destroyOne cardLens = do
  gs <- S.get
  gs ^. cardLens
    & map (Destroy cardLens)
    & resolveChoose

draw :: PlayerLens -> Game r ()
draw playerLens = do
  gs <- S.get
  playerLens . #hand ++= [topOfDeck playerLens gs]
  playerLens . #deck %= tail

topOfDeck :: PlayerLens -> GameState -> Card
topOfDeck playerLens = head . (^. playerLens . #deck)

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (b : bc)
  | a == b = bc
  | otherwise = b : deleteFirst a bc

resolveChoose :: [Action] -> Game r ()
resolveChoose actions = do
  maybeChoice <- Gio.chooseOne actions
  Gio.logLn' . show $ maybeChoice
  mapM_ resolve maybeChoice

modifiedField :: PlayerLens -> GameState -> [Card]
modifiedField playerLens gs =
  foldr (`resolveAura` playerLens) gs playerAuras
    & \gs ->
      foldr (`resolveAura` playerLens . otherPlayer gs) gs enemyAuras
        ^. playerLens . #field
  where
    getAuras = #field . traverse . #effects . #whileOnField
    playerAuras = gs ^. playerLens . getAuras
    enemyAuras = gs ^. playerLens . otherPlayer gs . getAuras

modified :: PlayerLens -> GameState -> Card -> Card
modified playerLens gs card =
  foldr applyAura card onFieldEffects
  where
    onFieldEffects = gs ^. playerLens . #field . traverse . #effects . #whileOnField

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