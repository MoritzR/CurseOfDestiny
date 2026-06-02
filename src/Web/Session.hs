{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Session (
  runGameServer,
) where

import Control.Exception (SomeException, handle)
import Data.Aeson (eitherDecode, encode)
import Data.Function ((&))
import Data.Text (pack, unpack)
import qualified Data.ByteString.Lazy as BL
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Local (State, evalState)
import DataTypes (GameState)
import Game (startGame)
import GameEffects (ChoiceInput (..), CommandInput (..), Log (..))
import GameState (getGameState, initialGameState)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, PendingConnection, acceptRequest, defaultConnectionOptions, receiveData, sendClose)
import qualified Network.WebSockets as WS
import Web.Protocol
import Web.Snapshot (snapshotGameState)

runGameServer :: IO ()
runGameServer =
  runSettings (setPort 8080 $ setHost "127.0.0.1" defaultSettings) $
    websocketsOr defaultConnectionOptions websocketApp httpFallback

websocketApp :: PendingConnection -> IO ()
websocketApp pending = do
  connection <- acceptRequest pending
  sendServerMessage connection $ Connected "Connected to CurseOfDestiny backend."
  handle
    (\(_ :: SomeException) -> sendClose connection ("bye" :: BL.ByteString))
    (runGameConnection connection)

httpFallback :: Application
httpFallback _ respond =
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "CurseOfDestiny WebSocket backend"

runGameConnection :: Connection -> IO ()
runGameConnection connection =
  runEff $
    startGame
      & runWebLog connection
      & runWebChoiceInput connection
      & runWebCommandInput connection
      & evalState initialGameState

runWebCommandInput :: (IOE :> es, State GameState :> es) => Connection -> Eff (CommandInput : es) a -> Eff es a
runWebCommandInput connection = interpret $ \_ -> \case
  ReadCommand prompt -> do
    sendSnapshot connection
    liftIO $ sendServerMessage connection $ PromptMessage $ CommandPrompt (pack prompt)
    liftIO $ awaitCommand connection

runWebChoiceInput :: (IOE :> es, State GameState :> es) => Connection -> Eff (ChoiceInput : es) a -> Eff es a
runWebChoiceInput connection = interpret $ \_ -> \case
  ChooseOnePrompt message options -> do
    sendSnapshot connection
    liftIO $ sendServerMessage connection $ PromptMessage $ ChoicePrompt (pack message) (pack <$> options)
    liftIO $ awaitChoice connection
  ReadChoice message -> do
    sendSnapshot connection
    liftIO $ sendServerMessage connection $ PromptMessage $ NumberPrompt (pack message)
    liftIO $ awaitNumber connection

runWebLog :: IOE :> es => Connection -> Eff (Log : es) a -> Eff es a
runWebLog connection = interpret $ \_ -> \case
  LogMessage message ->
    liftIO $ sendServerMessage connection $ NoticeMessage (pack message)

sendSnapshot :: (IOE :> es, State GameState :> es) => Connection -> Eff es ()
sendSnapshot connection = do
  state <- getGameState
  liftIO $ sendServerMessage connection $ StateSnapshot (snapshotGameState state)

sendServerMessage :: Connection -> ServerMessage -> IO ()
sendServerMessage connection = WS.sendTextData connection . encode

awaitClientMessage :: Connection -> IO ClientMessage
awaitClientMessage connection = do
  rawMessage <- (receiveData connection :: IO BL.ByteString)
  case eitherDecode rawMessage of
    Left errorMessage -> do
      sendServerMessage connection $ ErrorMessage $ pack $ "Invalid client message: " <> errorMessage
      awaitClientMessage connection
    Right clientMessage ->
      pure clientMessage

awaitCommand :: Connection -> IO String
awaitCommand connection = do
  message <- awaitClientMessage connection
  case message of
    SubmitCommand command ->
      pure $ unpack command
    _ -> do
      sendServerMessage connection $ ErrorMessage "Expected a command response."
      awaitCommand connection

awaitChoice :: Connection -> IO (Maybe Int)
awaitChoice connection = do
  message <- awaitClientMessage connection
  case message of
    SubmitChoice choiceIndex ->
      pure $ Just $ choiceIndex + 1
    _ -> do
      sendServerMessage connection $ ErrorMessage "Expected a choice response."
      awaitChoice connection

awaitNumber :: Connection -> IO Int
awaitNumber connection = do
  message <- awaitClientMessage connection
  case message of
    SubmitNumber number ->
      pure number
    _ -> do
      sendServerMessage connection $ ErrorMessage "Expected a numeric response."
      awaitNumber connection
