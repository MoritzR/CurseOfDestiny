module Actions (resolve, deleteFirst) where

import Control.Lens (over, (^.), (^..))
import Data.Function ((&))
import Data.Tuple (swap)
import DataTypes
import qualified GameIO as Gio
import qualified Polysemy.State as S
import PolysemyLens ((%=), (-=), (<>=))

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
  let actions = concat $ gs ^.. activePlayer . #field . traverse . #effects . #onTurnEnd
  mapM_ resolve actions

pass :: GameState -> GameState
pass = id

creaturePower :: Card -> Int
creaturePower card = case card ^. #cardType of
  Creature power -> power
  Spell -> 0 -- is a default a good idea here, or should this fail instead?

attack :: Card -> Card -> Game r ()
attack target source = case compare targetPower sourcePower of
  LT -> destroy (enemyPlayer . #field) target
  GT -> destroy (activePlayer . #field) source
  EQ -> do
    destroy (enemyPlayer . #field) target
    destroy (activePlayer . #field) source
  where
    (targetPower, sourcePower) = (creaturePower target, creaturePower source)

addToField :: Card -> Game r ()
addToField card = do
  activePlayer . #field <>= [card]

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
  playerLens . #hand <>= [topOfDeck playerLens gs]
  playerLens . #deck %= tail

topOfDeck :: PlayerLens -> GameState -> Card
topOfDeck playerLens = head . (^. playerLens . #deck)

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (b : bc)
  | a == b = bc
  | otherwise = b : deleteFirst a bc

resolveChoose :: [Action] -> Game r ()
resolveChoose l = do
  maybeChoice <- Gio.chooseOne l
  Gio.logLn' . show $ maybeChoice
  mapM_ resolve maybeChoice
