module Main where

import           Control.Monad.Trans            ( lift )
import           Data.Aeson
import           Data.ByteString.Lazy          as BS
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Discord
import qualified Discord.Requests              as R
import           Discord.Types
import           UnliftIO.Concurrent

import           Commands
import           Config
import           Reactions


-- For the time being, a lot of things are very similar to the discord-haskell example in its README
-- and the ping-pong example.

main :: IO ()
main = do
  TIO.putStrLn "Bot starting!"
  rawConfig <- BS.readFile "./app/config.json"
  let config = fromJust $ decode rawConfig
  userFacingError <- runDiscord $ def { discordToken   = configAuthToken config
                                      , discordOnStart = startHandler
                                      , discordOnEvent = eventHandler config
                                      }
  TIO.putStrLn userFacingError

startHandler :: DiscordHandler ()
startHandler = do
  let activity = Activity { activityName = "avansert rørlegging"
                          , activityType = ActivityTypeCompeting
                          , activityUrl  = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince     = Nothing
                              , updateStatusOptsGame      = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK       = False
                              }
  sendCommand (UpdateStatus opts)

eventHandler :: Config -> Event -> DiscordHandler ()
eventHandler c event = case event of
  MessageCreate      m -> messageHandler c m  -- Commands.hs
  MessageReactionAdd r -> reactionHandler r -- Reactions.hs
