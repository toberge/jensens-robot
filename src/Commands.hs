{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( isCommand
  , getCommand
  , getArgString
  , mentionsMe
  , execute
  , messageHandler
  ) where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Trans            ( lift )
import           Data.Aeson                     ( decode )
import           Data.ByteString.Builder        ( toLazyByteString )
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8Builder )
import qualified Data.Text.IO                  as TIO
import           Text.Printf

import           Discord
import qualified Discord.Requests              as R
import           Discord.Types

import           System.Process
import           System.Random

import           Config
import qualified Emoji                         as E
import           Lisp.Eval
import           Lisp.Types
import           McStatus
import           Quotes

-- Some helper functions

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

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

-- | Picks a random element of a list
-- Please ensure that the list has at least one element
-- TODO: Make sure this doesn't fail
choice :: [a] -> DiscordHandler a
choice options = do
  index <- lift $ randomRIO (0, length options - 1)
  pure $ options !! index

-- Event handler

messageHandler c m = unless (fromBot m) $ do
  when (mentionsMe (messageText m)) $ do
    restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
    pure ()
  -- threadDelay (4 * 10^6)
  when (isCommand (messageText m)) $ do
    execute c m
    pure ()

-- The commands!

commandList :: [(Text, Config -> Message -> DiscordHandler ())]
commandList =
  [ ("echo"     , echo)
  , ("ekko"     , echo)
  , ("ping"     , ping)
  , ("roll"     , roll)
  , ("cat"      , cats)
  , ("cats"     , cats)
  , ("katt"     , cats)
  , ("katter"   , cats)
  , ("quote"    , quote)
  , ("newQuote" , newQuote)
  , ("sitat"    , quote)
  , ("nyttSitat", newQuote)
  , ("donn"     , donn)
  , ("suggest"  , suggest)
  , ("foreslå"  , suggest)
  , ("mc"       , mcStatus)
  , ("help"     , help)
  , ("hjelp"    , help)
  , ("test"     , test)
  , ("blame"    , blame)
  , ("lisp"     , lisp)
  , ("lispHelp" , lispHelp)
  , ("lispHjelp", lispHelp)
  ]

commands = M.fromList commandList

execute c m = do
  maybe (reportError msg m) (\f -> f c m) res
 where
  res = M.lookup cmd commands
  cmd = getCommand $ messageText m
  msg = T.concat [T.pack E.bonk, " ", cmd, " er ingen kommando – prøv !hjelp"]

helpText =
  "`!echo <whatever>` sier det du vil tilbake, alias `!ekko`\n\
  \`!roll` for å kaste terning\n\
  \`!cats` for å se kattebilder (wip), alias `!katt`\n\
  \`!quote` for et sitat, alias `!sitat`\n\
  \`!newQuote <sitat> ; <opphav>` for å foreslå et sitat, alias `!nyttSitat`\n\
  \`!blame` for å legge skylda på noen andre\n\
  \`!suggest` for å foreslå en endring på serveren, alias `!foreslå`\n\
  \`!mc` viser status for Minecraft-serveren\n\
  \`!lisp <kode>` for å kjøre litt Lisp\n\
  \`!lispHelp` hvis du ikke har den fjerneste anelse om hva Lisp er\n\
  \- og et par andre kommandoer, kanskje <:gr:814410373724897281>"

reportError err m = do
  restCall (R.CreateMessage (messageChannel m) err)
  pure ()

help c m = do
  restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
    { createEmbedTitle       = "Rørleggeren støtter følgende kommandoer:"
    , createEmbedDescription = helpText
    }
  pure ()

ping c m = do
  restCall (R.CreateMessage (messageChannel m) "pong")
  pure ()

blame c m = do
  members' <- restCall
    (R.ListGuildMembers (fromJust $ messageGuild m)
                        (R.GuildMembersTiming (Just 100) Nothing)
    )
  case members' of
    Right members -> do
      -- Pick a random user
      member <- choice members
      let id = userId $ memberUser member
      -- Then blame them!
      restCall
        ( R.CreateMessage (messageChannel m)
        $ T.concat ["<@", T.pack $ show id, ">", " har skylda"]
        )
      pure ()
    Left (RestCallErrorCode code msg extra) -> do
      -- Just display the error...
      restCall
        ( R.CreateMessage (messageChannel m)
        $ T.concat [T.pack $ show code, " ", msg, " ", extra]
        )
      pure ()
  pure ()

suggest c m = do
  restCall
    $ R.CreateReaction (messageChannel m, messageId m) ":white_check_mark:"
  restCall $ R.CreateReaction (messageChannel m, messageId m)
                              ":negative_squared_cross_mark:"
  pure ()

cats c m = do
  cat <- choice E.cats
  restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
    { createEmbedTitle       = "Kattebilde, liksom"
    , createEmbedDescription = cat
    }
  pure ()

quote c m = do
  -- pickedQuote <- choice quotes
  pickedQuote <- lift $ readCreateProcess (shell "shuf -n 1 quotes") ""
  restCall $ R.CreateMessage (messageChannel m)
                             (formatQuote $ readQuote $ T.pack pickedQuote)
  pure ()

newQuote c m = do
  restCall $ R.CreateReaction (messageChannel m, messageId m) ":bookmark:"
  restCall $ R.CreateMessage
    (messageChannel m)
    "Reager med :bookmark: på meldinga ovenfor for å støtte forslaget"
  pure ()

donn c m = do
  pickedQuote <- choice donnJokes
  restCall $ R.CreateMessage (messageChannel m) (formatQuote pickedQuote)
  pure ()

mcStatus c m = do
  Right sm <- restCall
    $ R.CreateMessage (messageChannel m) "Sjekker status, vent litt..."
  rawStatus <- lift $ readCreateProcess
    (shell $ "mcstatus " ++ T.unpack (configMcServer c) ++ " json")
    ""
  let status =
        fromMaybe unknownMcStatus
        $ decode
        $ toLazyByteString
        $ encodeUtf8Builder
        $ T.pack rawStatus :: McStatus
  let playerText = T.concat
        [ T.pack $ show $ mcPlayerCount status
        , "/"
        , T.pack $ show $ mcPlayerMax status
        ]
  let onlineText = if mcIsOnline status
        then T.concat ["Online ", E.gamerfargen]
        else T.concat ["Offline ", E.brumm]
  restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
    { createEmbedTitle       = T.concat [E.minecraft, " ", configMcServer c]
    , createEmbedDescription = mcMotd status
    , createEmbedFields      = [ EmbedField "Status"   onlineText (Just True)
                               , EmbedField "Spillere" playerText (Just True)
                               ]
    }
  restCall $ R.DeleteMessage (messageChannel sm, messageId sm)
  pure ()

lisp c m = do
  let result = evalLisp $ T.unpack $ getArgString $ messageText m
  err <- choice E.errs
  case result of
    Left parseError ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [err, " Parse error"]
        , createEmbedDescription = T.pack $ show parseError
        }
    Right (Err evalError) ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [err, " Runtime error"]
        , createEmbedDescription = T.pack evalError
        }
    Right actualResult ->
      restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
        { createEmbedTitle       = T.concat [T.pack E.done, " Result"]
        , createEmbedDescription = T.concat
          ["```lisp\n", T.pack (showAST actualResult), "\n```"]
        }
  pure ()

lispHelp c m = do
  restCall $ R.CreateMessageEmbed (messageChannel m) "" $ def
    { createEmbedTitle       = "Hvordan funker dette? :thinking:"
    , createEmbedDescription = T.concat
      [ "`(i (lisp er (alt mulig) (inne (i parenteser))))`\n\n\
      \Prøv deg fram, noen funksjoner er vanlige symboler `(+ 1 1)` mens andre er emotes"
      , " `("
      , T.pack E.list
      , " 1 2 3)`.\n"
      , "(og jeg gadd ikke å la dere jobbe med annet enn lister og tall – enda)\n\n"
      , "Til slutt: Dette er _ikke_ en veldig sofistikert Lisp-variant, den har fint lite og ingenting."
      ]
    }
  pure ()

echo c m = do
  if length (T.words $ messageText m) > 1
    then restCall
      (R.CreateMessage (messageChannel m) (getArgString (messageText m)))
    else restCall
      (R.CreateMessage (messageChannel m) "Trenger et argument, kamerat")
  pure ()

roll c m = do
  num <- lift $ randomRIO (1, 6 :: Int)
  restCall
    (R.CreateMessage (messageChannel m)
                     (T.concat [":game_die: **", T.pack $ show num, "**"])
    )
  pure ()

test c m = do
  restCall
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
