module Reactions
  ( reactionHandler
  ) where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Trans            ( lift )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process

import           Discord
import qualified Discord.Requests              as R
import           Discord.Types

import           Commands                       ( getArgString
                                                , getCommand
                                                , isCommand
                                                )

reactionHandler r = do
    -- Handle bookmarking reactions
  when (emojiName (reactionEmoji r) == "🔖") $ do
    Right m <- restCall
      $ R.GetChannelMessage (reactionChannelId r, reactionMessageId r)
    let count = messageReactionCount
          (head $ filter (\r -> emojiName (messageReactionEmoji r) == "🔖")
                         (messageReactions m)
          )
    -- TODO check message content & make this have an actual effect
    let text = messageText m
    when
        (count > 4 && isCommand text && elem (getCommand text)
                                             ["newQuote", "nyttSitat"]
        )
      $ do
          let quote = getArgString text
          -- Ascertain that the quote has not been posted before
          (exitCode, _, _) <- lift $ readCreateProcessWithExitCode
            (shell $ "grep '" ++ T.unpack quote ++ "' quotes")
            ""
          lift $ TIO.putStrLn $ T.pack $ show exitCode
          when (exitCode /= ExitSuccess) $ do
            -- If the quote has not been added before, add it!
            lift $ TIO.appendFile "quotes" $ T.concat [quote, "\n"]
            restCall
              (R.CreateMessage (reactionChannelId r) $ T.concat
                [ "Sitatforslaget fra <@"
                , T.pack $ show $ userId (messageAuthor m)
                , "> har blitt lagt til"
                ]
              )
            pure ()
