module DescriptionSpec where

import Cards (series26)
import Data.List (intercalate)
import DataTypes (Card)
import Interpreter.Describe (describeCard)
import Test.Hspec
import Test.Hspec.Golden (golden)

spec :: Spec
spec = do
  describe "Golden tests for card descriptions" $ do
    golden "descriptions of set 26" $
      pure $
        renderCards series26

renderCards :: [Card] -> String
renderCards set = intercalate "\n\n" $ describeCard <$> set
