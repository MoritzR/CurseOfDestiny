module Cards where
import DataTypes

creature name power effects = Card name [Creature power] $ (OnPlay $ AddToField thisCard): effects
    where thisCard = creature name power effects

spell name effects = Card name [Spell] effects

dragonEgg = creature "Dragon Egg" 0 [OnTurnEnd $ AddToField dragon]
dragon = creature "Dragon" 2500 [OnPlay $ AddToField dragonEgg]
dog = creature "Dog" 1500 []
cat = creature "Cat" 500 []
catOrDog = spell "Cat or Dog?" [OnPlay $ Choose [AddToField dog, AddToField cat]]