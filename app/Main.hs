{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import Command

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- For the time being, a lot of things are very similar to the discord-haskell example in its README
-- and the ping-pong example.

main :: IO ()
main = do
    TIO.putStrLn "Bot starting!"
    token <- TIO.readFile "./auth-token.secret"
    userFacingError <- runDiscord $ def
        { discordToken   = token
        , discordOnStart = startHandler
        , discordOnEvent = eventHandler
        }
    TIO.putStrLn userFacingError

startHandler :: DiscordHandler ()
startHandler = do
    let activity = Activity { activityName = "avansert rørlegging"
                            , activityType = ActivityTypeGame
                            , activityUrl  = Nothing
                            }
    let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                                , updateStatusOptsGame = Just activity
                                , updateStatusOptsNewStatus = UpdateStatusOnline
                                , updateStatusOptsAFK = False
                                }
    sendCommand (UpdateStatus opts)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand (messageText m)) $ do
        _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        -- threadDelay (4 * 10^6)
        _ <- restCall (R.CreateMessage (messageChannel m) (parseAndExecute (messageText m)))
        pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
