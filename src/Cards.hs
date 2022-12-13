module Cards where

import DataTypes
  ( Action (AddToField, Choose, Destroy, DestroyOne, Draw),
    Aura (IncreaseAttack),
    Card (Card),
    CardEffects,
    CardType (Creature, Spell),
    PlayerCreature (PlayerCreature),
    activePlayer,
  )
import DataTypes qualified as T

noEffects :: CardEffects
noEffects = mempty

onPlay :: Action -> CardEffects
onPlay action = noEffects {T.onPlay = [action]}

onTurnEnd :: Action -> CardEffects
onTurnEnd action = noEffects {T.onTurnEnd = [action]}

onActivate :: Action -> CardEffects
onActivate action = noEffects {T.onActivate = [action]}

whileOnField :: Aura -> CardEffects
whileOnField aura = noEffects {T.whileOnField = [aura]}

creature id name power effects = Card (Creature power) id name $ onPlay (AddToField thisCard) <> effects
  where
    thisCard = creature id name power effects

spell = Card Spell

dog = creature "1" "Dog" 1500 noEffects

cat = creature "2" "Cat" 500 noEffects

catOrDog = spell "3" "Cat or Dog?" (onPlay $ Choose [AddToField dog, AddToField cat])

dragon = creature "4" "Dragon" 2500 (onPlay $ AddToField dragonEgg)

dragonEgg = creature "5" "Dragon Egg" 0 (onTurnEnd (AddToField dragon) <> onTurnEnd (Destroy (activePlayer . #field) dragonEgg))

catFactory = creature "6" "Cat Factory" 500 (onActivate $ AddToField cat)

masterOfGreed = creature "7" "Master of Greed" 500 (onActivate (DestroyOne $ activePlayer . #field) <> onActivate (Draw activePlayer))

buff = creature "8" "Mr. Buff" 0 (whileOnField (IncreaseAttack 500))

defaultPlayerCreature = PlayerCreature "1" 7