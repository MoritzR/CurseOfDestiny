module GameIO where

import Effectful (Eff, (:>))
import GameEffects (ChoiceInput, Log, chooseOnePrompt, logMessage)

chooseOne :: (ChoiceInput :> es, Show a) => [a] -> Eff es (Maybe a)
chooseOne l = do
  choice <- chooseOnePrompt "Choose one:" (show <$> l)
  pure $ choice >>= \picked -> atMay l (picked - 1)

displayEnumeratedItems :: (Log :> es, Show a) => [a] -> Eff es ()
displayEnumeratedItems = mapM_ displayTuple . zip [1 :: Int ..]
 where
  displayTuple (i, v) = log' $ show i ++ ": " ++ show v

log' :: Log :> es => String -> Eff es ()
log' = logMessage

logLn' :: Log :> es => String -> Eff es ()
logLn' s = logMessage $ "\n" ++ s

atMay :: [a] -> Int -> Maybe a
atMay items index
  | index < 0 = Nothing
  | otherwise = go items index
 where
  go [] _ = Nothing
  go (item : _) 0 = Just item
  go (_ : rest) n = go rest (n - 1)
