{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module GameEffects
  ( ChoiceInput,
    CommandInput,
    Log,
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

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful (liftIO)
import Effectful.Dispatch.Dynamic (interpret, send)

data ChoiceInput :: Effect where
  ReadChoice :: ChoiceInput m Int

type instance DispatchOf ChoiceInput = 'Dynamic

readChoice :: (ChoiceInput :> es) => Eff es Int
readChoice = send ReadChoice

runChoiceInputConst :: Int -> Eff (ChoiceInput : es) a -> Eff es a
runChoiceInputConst n = interpret $ \_ -> \case
  ReadChoice -> pure n

runChoiceInputIO :: (IOE :> es) => IO Int -> Eff (ChoiceInput : es) a -> Eff es a
runChoiceInputIO getChoice = interpret $ \_ -> \case
  ReadChoice -> liftIO getChoice

data CommandInput :: Effect where
  ReadCommand :: CommandInput m String

type instance DispatchOf CommandInput = 'Dynamic

readCommand :: (CommandInput :> es) => Eff es String
readCommand = send ReadCommand

runCommandInputIO :: (IOE :> es) => IO String -> Eff (CommandInput : es) a -> Eff es a
runCommandInputIO getCommand = interpret $ \_ -> \case
  ReadCommand -> liftIO getCommand

data Log :: Effect where
  LogMessage :: String -> Log m ()

type instance DispatchOf Log = 'Dynamic

logMessage :: (Log :> es) => String -> Eff es ()
logMessage = send . LogMessage

ignoreLog :: Eff (Log : es) a -> Eff es a
ignoreLog = interpret $ \_ -> \case
  LogMessage _ -> pure ()

runLogToIO :: (IOE :> es) => (String -> IO ()) -> Eff (Log : es) a -> Eff es a
runLogToIO writeLog = interpret $ \_ -> \case
  LogMessage message -> liftIO (writeLog message)
