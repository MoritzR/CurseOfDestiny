module Cards where

import DataTypes
  ( Action (AddToField, Choose, Destroy, DestroyOne, Draw),
    Card (Card),
    CardEffects,
    CardType (Creature, Spell),
    PlayerCreature (PlayerCreature),
    activePlayer,
  )
import qualified DataTypes as T

onPlay :: Action -> CardEffects
onPlay actions = mempty {T.onPlay = [actions]}

onTurnEnd :: Action -> CardEffects
onTurnEnd actions = mempty {T.onTurnEnd = [actions]}

onActivate :: Action -> CardEffects
onActivate actions = mempty {T.onActivate = [actions]}

noEffects :: CardEffects
noEffects = mempty

creature id name power effects = Card id name (Creature power) $ onPlay (AddToField thisCard) <> effects
  where
    thisCard = creature id name power effects

spell id name effects = Card id name Spell effects

dog = creature "1" "Dog" 1500 noEffects

cat = creature "2" "Cat" 500 noEffects

catOrDog = spell "3" "Cat or Dog?" (onPlay $ Choose [AddToField dog, AddToField cat])

dragon = creature "4" "Dragon" 2500 (onPlay $ AddToField dragonEgg)

dragonEgg = creature "5" "Dragon Egg" 0 (onTurnEnd (AddToField dragon) <> onTurnEnd (Destroy (activePlayer . #field) dragonEgg))

catFactory = creature "6" "Cat Factory" 500 (onActivate $ AddToField cat)

masterOfGreed = creature "7" "Master of Greed" 500 (onActivate (DestroyOne $ activePlayer . #field) <> onActivate (Draw activePlayer))

defaultPlayerCreature = PlayerCreature "1" 7