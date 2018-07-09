module Cards where
import DataTypes

creature name effects = Card name $ (OnPlay $ AddToField thisCard): effects
    where thisCard = creature name effects

spell name effects = Card name effects

dragonEgg = creature "Dragon Egg" [OnTurnEnd $ AddToField dragon]
dragon = creature "Dragon" [OnPlay $ AddToField dragonEgg]
dog = creature "Dog" []
cat = creature "Cat" []
catOrDog = spell "Cat or Dog?" [OnPlay $ Choose [AddToField dog, AddToField cat]]