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

import qualified Emoji                         as E
import           Lisp.Eval
import           Lisp.Types

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
getArgString text = maybe "" (\pos -> snd $ T.splitAt (pos + 1) text) maybePos
  where maybePos = T.findIndex (== ' ') text

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
  , ("lisp" , lisp)
  ]

commands = M.fromList commandList

execute m = do
  maybe (reportError "Det fins ingen kommando" m) (\f -> f m) res
 where
  res = M.lookup cmd commands
  cmd = getCommand $ messageText m

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

lisp m = do
  _ <- case result of
    Left parseError ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [T.pack E.err, " Parse error"]
        , createEmbedDescription = T.pack $ show parseError
        }
    Right (Err evalError) ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [T.pack E.err, " Runtime error"]
        , createEmbedDescription = T.pack evalError
        }
    Right actualResult ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [T.pack E.done, " Result"]
        , createEmbedDescription = T.concat
          ["```lisp\n", T.pack (showAST actualResult), "\n```"]
        }


  pure ()
  where result = evalLisp $ T.unpack $ getArgString $ messageText m

echo m = do
  _ <- if length (T.words $ messageText m) > 1
    then restCall
      (R.CreateMessage (messageChannel m) (getArgString (messageText m)))
    else restCall
      (R.CreateMessage (messageChannel m) "Trenger et argument, kamerat")
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
