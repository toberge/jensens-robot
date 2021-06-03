module Config where

import           Data.Aeson
import           Data.Text                      ( Text )

data Config = Config
  { configAuthToken :: Text
  , configPrefix    :: Text
  , configMcServer  :: Text
  }
  deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config"
    $ \v -> Config <$> v .: "authToken" <*> v .: "prefix" <*> v .: "mcServer"
