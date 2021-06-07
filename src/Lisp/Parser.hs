module Lisp.Parser
  ( parseLispMaybe
  , parseLisp
  ) where

import           Data.Char                      ( toLower )
import           Data.List                      ( find
                                                , intercalate
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Parsec
import           Text.Printf

import qualified Emoji                         as E
import           Lisp.Types

type Parser = Parsec Text ()
type GenParser t st = Parsec Text st

text = string . T.unpack

parseLispMaybe :: Text -> Maybe AST
parseLispMaybe = rightToMaybe . parseLisp

parseLisp :: Text -> Either ParseError AST
parseLisp = parse parseAST "(jensens lisp)"

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _  ) = Nothing
rightToMaybe (Right val) = Just val

whitespace :: Parser ()
whitespace = skipMany $ oneOf ",\r\n\t "

lexeme :: Parser a -> Parser a
lexeme parser = do
  value <- parser
  whitespace
  return value

parseAST :: Parser AST
parseAST = do
  whitespace
  asts <- many1 parseSequence
  eof
  return $ Seq asts

parseSequence :: Parser AST
parseSequence = do
  ast <- parseExpr
  whitespace
  return ast

parseList :: Parser AST
parseList = do
  lexeme $ char '('
  lexeme $ text E.list
  values <- many $ lexeme parseExpr
  lexeme $ char ')'
  return $ Lst values

parseNode :: Parser AST
parseNode = do
  lexeme $ char '('
  first  <- lexeme parseExpr
  values <- many $ lexeme parseExpr
  lexeme $ char ')'
  return $ Nod first values

parseExpr :: Parser AST
parseExpr = do
  parseNumber
    <|> try parseNul
    <|> try parseBool
    <|> try parseSymbol
    <|> parseNode

parseSymbol :: Parser AST
parseSymbol = do
  start <- noneOf $ " ,\n\t\r()" <> ['0' .. '9']
  rest  <- many $ noneOf " ,\n\t\r()"
  return $ Sym $ T.pack $ start : rest

parseNumber :: Parser AST
parseNumber = do
  num <- many1 (oneOf "1234567890")
  return $ I32 $ read num

parseNul :: Parser AST
parseNul = do
  text E.null <|> parseEmptyBrackets
  return Nul

parseEmptyBrackets :: Parser String
parseEmptyBrackets = do
  lexeme $ char '('
  lexeme $ char ')'
  return "()" -- at least this fixed it... idk

parseBool :: Parser AST
parseBool = do
  value <- text E.true <|> text E.false
  return $ Boo $ value == T.unpack E.true
