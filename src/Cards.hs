module Cards where
import DataTypes
import Control.Lens ((^.))

creature id name power effects = Card id name (Creature power) $ (OnPlay $ AddToField thisCard): effects
    where thisCard = creature id name power effects

spell id name effects = Card id name Spell effects

dog = creature "1" "Dog" 1500 []
cat = creature "2" "Cat" 500 []
catOrDog = spell "3" "Cat or Dog?" [OnPlay $ Choose [AddToField dog, AddToField cat]]
dragon = creature "4" "Dragon" 2500 [OnPlay $ AddToField dragonEgg]
dragonEgg = creature "5" "Dragon Egg" 0 [OnTurnEnd $ AddToField dragon, OnTurnEnd $ Destroy (activePlayer.field) dragonEgg]
catFactory = creature "6" "Cat Factory" 500 [OnActivate $ AddToField cat]
masterOfGreed = creature "7" "Master of Greed" 500 [OnActivate $ DestroyOne $ activePlayer.field, OnActivate $ Draw $ activePlayer]

defaultPlayerCreature = PlayerCreature "1" 7