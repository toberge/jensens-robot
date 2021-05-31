{-# LANGUAGE OverloadedStrings #-}

module Command
  ( isCommand,
    getCommand,
    parseAndExecute,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Printf

data Command
  = Echo Text
  | Ping
  deriving (Show, Eq)

isCommand :: Text -> Bool
isCommand = ("!" `T.isPrefixOf`)

getCommand :: Text -> Text
getCommand = T.drop 1 . head . T.words

-- stolen from StackOverflow
type Parser = Parsec T.Text ()

type GenParser t st = Parsec T.Text st

-- Wrapper funs
parseAndExecute :: Text -> Text
parseAndExecute cmd = either (T.pack . show) exec $ parseStuff cmd

parseMaybe :: Text -> Maybe Command
parseMaybe = rightToMaybe . parseStuff

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right val) = Just val

parseStuff :: Text -> Either ParseError Command
parseStuff = parse parseCommand "(what)"

whitespace = skipMany $ oneOf " \t\n\r"

parseCommand :: Parser Command
parseCommand = do
  _ <- char '!'
  choice [parsePing, parseEcho]

parseArgs = many (whitespace *> many1 (noneOf " "))

parseInt :: Parser Int
parseInt = do
  num <- many1 (oneOf "1234567890")
  return $ read num

parsePing :: Parser Command
parsePing = do
  _ <- string "ping"
  return Ping

parseEcho :: Parser Command
parseEcho = do
  _ <- string "echo"
  args <- parseArgs -- todo these are not just args though
  return $ Echo (T.unwords $ map T.pack args)

exec :: Command -> Text
exec (Echo text) = text
exec Ping = "pong"
