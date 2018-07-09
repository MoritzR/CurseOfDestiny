module Cards where
import DataTypes

creature name effects = Card name $ (OnPlay $ AddToField thisCard): effects
    where thisCard = creature name effects

spell name effects = Card name effects