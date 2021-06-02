{-# LANGUAGE OverloadedStrings #-}

module McStatus where

import           Data.Aeson
import           Data.Text                      ( Text )

data McStatus = McStatus
  { mcIsOnline    :: Bool
  , mcMotd        :: Text
  , mcPlayerCount :: Int
  , mcPlayerMax   :: Int
  }

unknownMcStatus = McStatus False "An error occurred" 0 0

instance FromJSON McStatus where
  parseJSON = withObject "McStatus" $ \v -> do
    motd <- v .: "motd"
    McStatus
      <$> v
      .:  "online"
      <*> motd
      .:  "text"
      <*> v
      .:  "player_count"
      <*> v
      .:  "player_max"
