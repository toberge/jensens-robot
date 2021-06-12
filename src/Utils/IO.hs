module Utils.IO where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Random

filterLines query = filter (query `T.isInfixOf`)

readLines :: Text -> IO [Text]
readLines fileName = do
  text <- TIO.readFile $ T.unpack fileName
  return $ T.lines text

grep :: Text -> Text -> IO [Text]
grep fileName query = do
  lines <- readLines fileName
  return $ filterLines query lines

-- | Picks a random element of a list
-- Please ensure that the list has at least one element
randomChoice :: [a] -> IO a
randomChoice options = do
  index <- randomRIO (0, length options - 1)
  return $ options !! index

maybeChoice :: [a] -> IO (Maybe a)
maybeChoice [] = return Nothing
maybeChoice xs = do
  x <- randomChoice xs
  return $ Just x

pickGrep :: Text -> Text -> IO (Maybe Text)
pickGrep fileName query = do
  lines <- grep fileName query
  maybeChoice lines
