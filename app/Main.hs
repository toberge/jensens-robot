-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Trans            ( lift )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Discord
import qualified Discord.Requests              as R
import           Discord.Types
import           UnliftIO.Concurrent

-- For the time being, a lot of things are very similar to the discord-haskell example in its README
-- and the ping-pong example.

main :: IO ()
main = do
  TIO.putStrLn "Bot starting!"
  token           <- TIO.readFile "./auth-token.secret"
  userFacingError <- runDiscord $ def { discordToken   = token
                                      , discordOnStart = startHandler
                                      , discordOnEvent = eventHandler
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

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> unless (fromBot m) $ do
    when (mentionsMe (messageText m)) $ do
      _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
      pure ()
    -- threadDelay (4 * 10^6)
    when (isCommand (messageText m)) $ do
      _ <- execute m
      pure ()

  MessageReactionAdd r -> do
    -- Handle bookmarking reactions (TODO split into its own file if it becomes big)
    when (emojiName (reactionEmoji r) == "🔖") $ do
      Right m <- restCall
        $ R.GetChannelMessage (reactionChannelId r, reactionMessageId r)
      let count = messageReactionCount
            (head $ filter (\r -> emojiName (messageReactionEmoji r) == "🔖")
                           (messageReactions m)
            )
      -- TODO check message content & make this have an actual effect
      when (count > 4) $ do
        _ <- restCall
          (R.CreateMessage (reactionChannelId r) $ T.concat
            [ "Sitatforslaget fra <@"
            , T.pack $ show $ userId (messageAuthor m)
            , "> bør legges til"
            ]
          )
        pure ()


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
