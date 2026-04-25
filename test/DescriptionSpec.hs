module DescriptionSpec where


import Test.Hspec
import Cards (series26)
import DataTypes (Card)
import Test.Hspec.Golden (golden)
import Interpreter.Descriptor (describeCard)
import Data.List (intercalate)

spec :: Spec
spec = do
  describe "Golden tests for card descriptions" $ do
    golden "descriptions of set 26" $
      pure $ renderCards series26

renderCards :: [Card] -> String
renderCards set = intercalate "\n\n" $ describeCard <$> set

