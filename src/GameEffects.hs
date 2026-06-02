{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module GameEffects (
  ChoiceInput (..),
  CommandInput (..),
  Log (..),
  chooseOnePrompt,
  ignoreLog,
  logMessage,
  readChoice,
  readCommand,
  runChoiceInputConst,
  runChoiceInputIO,
  runCommandInputIO,
  runLogToIO,
)
where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

data ChoiceInput :: Effect where
  ChooseOnePrompt :: String -> [String] -> ChoiceInput m (Maybe Int)
  ReadChoice :: String -> ChoiceInput m Int

type instance DispatchOf ChoiceInput = 'Dynamic

chooseOnePrompt :: ChoiceInput :> es => String -> [String] -> Eff es (Maybe Int)
chooseOnePrompt message options = send $ ChooseOnePrompt message options

readChoice :: ChoiceInput :> es => String -> Eff es Int
readChoice = send . ReadChoice

runChoiceInputConst :: Int -> Eff (ChoiceInput : es) a -> Eff es a
runChoiceInputConst n = interpret $ \_ -> \case
  ChooseOnePrompt _ options
    | n >= 1 && n <= length options -> pure $ Just n
    | otherwise -> pure Nothing
  ReadChoice _ -> pure n

runChoiceInputIO :: IOE :> es => IO Int -> Eff (ChoiceInput : es) a -> Eff es a
runChoiceInputIO getChoice = interpret $ \_ -> \case
  ChooseOnePrompt message options -> do
    liftIO $ putStrLn message
    liftIO $ mapM_ putStrLn [show i <> ": " <> option | (i, option) <- zip [1 :: Int ..] options]
    picked <- liftIO getChoice
    pure $
      if picked >= 1 && picked <= length options
        then Just picked
        else Nothing
  ReadChoice _ -> liftIO getChoice

data CommandInput :: Effect where
  ReadCommand :: String -> CommandInput m String

type instance DispatchOf CommandInput = 'Dynamic

readCommand :: CommandInput :> es => String -> Eff es String
readCommand = send . ReadCommand

runCommandInputIO :: IOE :> es => IO String -> Eff (CommandInput : es) a -> Eff es a
runCommandInputIO getCommand = interpret $ \_ -> \case
  ReadCommand prompt -> liftIO (putStrLn prompt) *> liftIO getCommand

data Log :: Effect where
  LogMessage :: String -> Log m ()

type instance DispatchOf Log = 'Dynamic

logMessage :: Log :> es => String -> Eff es ()
logMessage = send . LogMessage

ignoreLog :: Eff (Log : es) a -> Eff es a
ignoreLog = interpret $ \_ -> \case
  LogMessage _ -> pure ()

runLogToIO :: IOE :> es => (String -> IO ()) -> Eff (Log : es) a -> Eff es a
runLogToIO writeLog = interpret $ \_ -> \case
  LogMessage message -> liftIO (writeLog message)
