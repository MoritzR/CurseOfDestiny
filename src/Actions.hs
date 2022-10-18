{-# LANGUAGE LambdaCase #-}

module Actions (resolve, creaturePower, modifiedField) where

import Control.Lens (over, (^.), (^..))
import Control.Lens.Setter (set)
import Data.Function ((&))
import Data.Generics.Sum.Constructors (_Ctor')
import Data.Generics.Sum.Subtype (_Sub)
import Data.Tuple (swap)
import DataTypes
import qualified GameIO as Gio
import qualified Polysemy.State as S
import PolysemyLens ((%=), (-=), (++=), use)
import Data.List (delete)
import Polysemy (Sem)

resolve :: HasStateIO r => Action -> Sem r ()
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

applyTurnEnds :: HasStateIO r => Sem r ()
applyTurnEnds = do
  actions <- use $ activePlayer . #field . traverse . #effects . #onTurnEnd
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
  cardLens %= delete card

directAttack :: Card -> PlayerLens -> Game r ()
directAttack _source targetPlayer = do
  targetPlayer . playerHp -= 1

discardFromHand :: Card -> Game r ()
discardFromHand card = do
  activePlayer . #hand %= delete card

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

resolveChoose :: HasStateIO r => [Action] -> Sem r ()
resolveChoose actions = do
  maybeChoice <- Gio.chooseOne actions
  Gio.logLn' . show $ maybeChoice
  mapM_ resolve maybeChoice

modifiedField :: PlayerLens -> GameState -> [Card]
modifiedField playerLens gs =
  map (modified playerLens gs) field
  where
    field = gs ^. playerLens . #field

modified :: PlayerLens -> GameState -> Card -> Card
modified playerLens gs card =
  foldr applyAura card onFieldEffects
  where
    onFieldEffects = gs ^. playerLens . #field . traverse . #effects . #whileOnField

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