{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Protocol where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options (..), SumEncoding (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data ClientMessage
  = SubmitCommand {command :: Text}
  | SubmitChoice {choiceIndex :: Int}
  | SubmitNumber {number :: Int}
  deriving (Eq, Show, Generic)

instance FromJSON ClientMessage where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientMessage where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data ServerMessage
  = Connected {message :: Text}
  | StateSnapshot {state :: GameSnapshot}
  | PromptMessage {prompt :: Prompt}
  | NoticeMessage {message :: Text}
  | ErrorMessage {message :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON ServerMessage where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data Prompt
  = CommandPrompt {message :: Text}
  | ChoicePrompt {message :: Text, options :: [Text]}
  | NumberPrompt {message :: Text}
  deriving (Eq, Show, Generic, ToJSON)

data GameSnapshot = GameSnapshot
  { currentPlayer :: Text
  , nextCardId :: Int
  , players :: [PlayerSnapshot]
  }
  deriving (Eq, Show, Generic, ToJSON)

data PlayerSnapshot = PlayerSnapshot
  { playerId :: Text
  , name :: Text
  , schicksalsmacht :: Int
  , hand :: [CardSnapshot]
  , field :: [CardSnapshot]
  , graveyard :: [CardSnapshot]
  , deck :: [CardSnapshot]
  }
  deriving (Eq, Show, Generic, ToJSON)

data CardSnapshot = CardSnapshot
  { cardId :: Int
  , owner :: Text
  , name :: Text
  , cost :: Text
  , cardType :: Text
  , baseStrength :: Maybe Int
  , currentStrength :: Maybe Int
  , tags :: [Text]
  , description :: Text
  , modifications :: [ModificationSnapshot]
  }
  deriving (Eq, Show, Generic, ToJSON)

data ModificationSnapshot = ModificationSnapshot
  { kind :: Text
  , duration :: Text
  , amount :: Maybe Int
  , description :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { sumEncoding = TaggedObject "type" "payload"
    }
