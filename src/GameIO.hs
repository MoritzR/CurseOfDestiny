module GameIO where

import Polysemy (Member, Members, Sem)
import Polysemy.Input (Input, input)
import Polysemy.Trace (Trace, trace)

chooseOne :: (Members [Trace, Input Int] r, Show a) => [a] -> Sem r (Maybe a)
chooseOne l = do
  displayEnumeratedItems l
  log' "Choose one: "
  choice <- input
  if choice < 1 || choice > length l
    then return Nothing
    else return $ Just (l !! (choice - 1))

displayEnumeratedItems :: (Member Trace r, Show a) => [a] -> Sem r ()
displayEnumeratedItems = mapM_ displayTuple . zip [1 ..]
  where
    displayTuple (i, v) = log' $ show i ++ ": " ++ show v

log' :: Member Trace r => String -> Sem r ()
log' = trace

logLn' :: Member Trace r => String -> Sem r ()
logLn' s = trace $ "\n" ++ s