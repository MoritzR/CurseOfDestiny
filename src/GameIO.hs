module GameIO where

import Effectful (Eff, (:>))
import GameEffects (ChoiceInput, Log, logMessage, readChoice)

chooseOne :: (Log :> es, ChoiceInput :> es, Show a) => [a] -> Eff es (Maybe a)
chooseOne l = do
  displayEnumeratedItems l
  log' "Choose one: "
  choice <- readChoice
  if choice < 1 || choice > length l
    then return Nothing
    else return $ Just (l !! (choice - 1))

displayEnumeratedItems :: (Log :> es, Show a) => [a] -> Eff es ()
displayEnumeratedItems = mapM_ displayTuple . zip [1 ..]
  where
    displayTuple (i, v) = log' $ show i ++ ": " ++ show v

log' :: (Log :> es) => String -> Eff es ()
log' = logMessage

logLn' :: (Log :> es) => String -> Eff es ()
logLn' s = logMessage $ "\n" ++ s
