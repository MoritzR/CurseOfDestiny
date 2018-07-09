module Cards where
import DataTypes

creature name (attack, defense) effects = Card name [Creature attack defense] $ (OnPlay $ AddToField thisCard): effects
    where thisCard = creature name (attack, defense) effects

spell name effects = Card name [Spell] effects

dragonEgg = creature "Dragon Egg" (0, 100) [OnTurnEnd $ AddToField dragon]
dragon = creature "Dragon" (2500, 2000) [OnPlay $ AddToField dragonEgg]
dog = creature "Dog" (1500, 1500) []
cat = creature "Cat" (1800, 500)[]
catOrDog = spell "Cat or Dog?" [OnPlay $ Choose [AddToField dog, AddToField cat]]