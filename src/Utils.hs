module Utils where

import           Data.Semigroup
import           Data.Text                      ( Text
                                                , isPrefixOf
                                                )
import qualified Data.Text                     as T
import           Discord.Types

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

mention :: User -> Text
mention u = "<@" <> T.pack (show $ userId u) <> ">"

mentionAuthor :: Message -> Text
mentionAuthor m = mention (messageAuthor m)

messageAuthorId :: Message -> Snowflake
messageAuthorId m = userId (messageAuthor m)

isMention :: Text -> Bool
isMention = isPrefixOf "<@"
