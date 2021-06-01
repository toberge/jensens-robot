{-# LANGUAGE OverloadedStrings #-}

module Command
  ( isCommand
  , getCommand
  , mentionsMe
  , execute
  ) where

import           Control.Monad.Trans            ( lift )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Printf

import           Discord
import qualified Discord.Requests              as R
import           Discord.Types

import           System.Random

-- Some helper functions

isCommand :: Text -> Bool
isCommand = ("!" `T.isPrefixOf`)

mentionsMe :: Text -> Bool
mentionsMe text' = count > 0
 where
  count = T.count "rørlegger" text + T.count "bot" text
  text  = T.toLower text'

getCommand :: Text -> Text
getCommand = T.drop 1 . head . T.words

getArgString :: Text -> Text
getArgString = T.unwords . tail . T.words

-- The commands!

commandList :: [(Text, Message -> DiscordHandler ())]
commandList =
  [ ("echo" , echo)
  , ("ekko" , echo)
  , ("ping" , ping)
  , ("roll" , roll)
  , ("help" , help)
  , ("hjelp", help)
  , ("test" , test)
  ]

commands = M.fromList commandList

execute m = do
  maybe (reportError "No such command" m) (\f -> f m) res
  where res = M.lookup (getCommand $ messageText m) commands

helpText =
  "Rørleggeren støtter følgende kommandoer:\n\
  \`echo <whatever>` sier det du vil tilbake, alias `ekko`\n\
  \`roll` for å kaste terning\n\
  \`ping` ... pong"

reportError err m = do
  _ <- restCall (R.CreateMessage (messageChannel m) err)
  pure ()

help m = do
  _ <- restCall (R.CreateMessage (messageChannel m) helpText)
  pure ()

ping m = do
  _ <- restCall (R.CreateMessage (messageChannel m) "pong")
  pure ()

echo m = do
  _ <- if length (T.words $ messageText m) > 1
    then restCall
      (R.CreateMessage (messageChannel m) (getArgString (messageText m)))
    else restCall
      (R.CreateMessage (messageChannel m) "Echo requires an argument")
  pure ()

roll m = do
  num <- lift $ randomRIO (1, 6 :: Int)
  _   <- restCall
    (R.CreateMessage (messageChannel m)
                     (T.concat [":game_die: **", T.pack $ show num, "**"])
    )
  pure ()

test m = do
  _ <- restCall
    (R.CreateMessageEmbed (messageChannel m) "" $ def
      { createEmbedTitle       = "Jensens rørleggerservice"
      , createEmbedDescription = "Vi leverer bare rør"
      , createEmbedThumbnail   =
        Just
          $ CreateEmbedImageUrl
              "https://files.solvecms.com/test/7311e68/medium/jensen%20r%C3%B8rleggerservice%20logo.jpg?v=1519740759023"
      , createEmbedImage       =
        Just
          $ CreateEmbedImageUrl
              "https://files.solvecms.com/test/7311e68/medium/jensen%20r%C3%B8rleggerservice%20logo.jpg?v=1519740759023"
      , createEmbedFields      = [ EmbedField "Dette" "er"   (Just True)
                                 , EmbedField "en"    "test" (Just True)
                                 , EmbedField "eller" "hva?" (Just True)
                                 ]
      }
    )
  pure ()
