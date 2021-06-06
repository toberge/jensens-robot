module Lisp.Eval
  ( showAST
  , evalLisp
  , evalLispToString
  , builtinNames
  ) where

import           Data.Char                      ( toLower )
import           Data.List                      ( find )
import qualified Data.Map                      as M
import           Data.Semigroup
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Parsec                    ( ParseError )

import qualified Emoji                         as E
import           Lisp.Parser
import           Lisp.Types

-- This Lisp-esque language was inspired by https://www.codewars.com/kata/598a82f07bad362e1d000003
-- and implements a limited set of functions

-- TODO refactor to Text and clean up this mess

-- -- -- Functions -- -- --

eval :: AST -> AST
eval (Nod (Sym sym) args) = maybe (Err $ sym <> " er ingen kjent funksjon")
                                  (\f -> f args)
                                  fun
  where fun = M.lookup sym builtins
eval (Lst values) = Lst $ map eval values
eval value        = value

evalNumber :: AST -> AST
evalNumber num@(I32 x) = num
evalNumber ast         = case res of
  num@(I32 x) -> num
  _           -> Err "Trenger et tall"
  where res = eval ast

-- Wrapper for *boolean* ops
lispBinaryOp name op = fun
 where
  fun [I32 a, I32 b] = Boo $ op a b
  fun [Err x, _    ] = Err x
  fun [_    , Err x] = Err x
  fun [a    , b    ] = fun [evalNumber a, evalNumber b]
  fun _              = Err $ name <> " trenger to tall"

-- Wrapper for binary ops, but generic
lispBinaryOpGeneric name op = fun
 where
  fun [I32 a, I32 b] = op a b
  fun [Err x, _    ] = Err x
  fun [_    , Err x] = Err x
  fun [a    , b    ] = fun [evalNumber a, evalNumber b]
  fun _              = Err $ name <> " trenger to tall"

isI32 (I32 _) = True
isI32 _       = False

isList (Lst _) = True
isList _       = False

isValid (Err _) = False
isValid _       = True

getList (Lst xs) = xs
getList _        = [Err "wtf"]

-- Wrapper for binary functions that should work on multiple values
lispMultiOp :: Text -> (Int -> Int -> Int) -> ([AST] -> AST)
lispMultiOp name op = fun
 where
  fun [] = Err $ name <> " trenger argumenter"
  fun xs' | all isI32 xs = I32 $ foldl1 op $ map (\(I32 x) -> x) xs
          | otherwise    = Err $ name <> " kan bare motta tall som argumenter"
    where xs = map eval xs'

lispNot [Boo bool] = Boo $ not bool
lispNot [ast     ] = case eval ast of
  (Boo bool) -> Boo $ not bool
  _          -> Err $ E.nope <> " kan bare brukes på boolske verdier"
lispNot _ = Err $ E.nope <> " trenger ett argument"

lispList = Lst . map eval

lispSize [node] = case eval node of
  (Lst xs) -> I32 $ length xs
  _        -> Err "Kan ikke finne størrelsen til noe som ikke er ei liste"
lispSize _ = Err "Kan bare finne størrelsen til én ting"

lispReverse [node] = case eval node of
  (Lst xs) -> Lst $ reverse xs
  _        -> Err "Kan ikke reversere noe som ikke er ei liste"
lispReverse _ = Err "Kan bare reversere ei liste, verken mer eller mindre"

lispMember [x', list']
  | isList list && isValid x = Boo $ elem x $ getList list
  | otherwise = Err $ E.member <> " kan bare brukes på element og liste"
 where
  list = eval list'
  x    = eval x'


lispRange = lispBinaryOpGeneric ".." range
  where range a b = Lst (map I32 [a .. b])

lispPower = lispBinaryOpGeneric "^" pow where pow a b = I32 $ a ^ b

lispIf :: [AST] -> AST
lispIf [Boo x, a]    = if x then a else Nul
lispIf [Boo x, a, b] = if x then a else b
lispIf (x : xs)      = case res of
  bool@(Boo _) -> lispIf (bool : xs)
  _            -> Err $ E.whatIf <> " krever boolsk verdi som første argument"
  where res = eval x
lispIf _ = Err $ "Ugyldig argument til " <> E.whatIf

builtinList :: [(Text, [AST] -> AST)]
builtinList =
  [ (E.plus          , lispMultiOp E.plus (+))
  , (E.minus         , lispMultiOp E.minus (-))
  , (E.times         , lispMultiOp E.times (*))
  , (E.divide        , lispMultiOp E.divide div)
  , (E.power         , lispPower)
  , (E.greater       , lispBinaryOp E.greater (>))
  , (E.less          , lispBinaryOp E.less (<))
  , (E.nope          , lispNot)
  , (E.list          , lispList)
  , (E.size          , lispSize)
  , (E.reverse       , lispReverse)
  , (E.member        , lispMember)
  , (E.range         , lispRange)
  , (E.equal         , lispBinaryOp E.equal (==))
  , (E.greaterOrEqual, lispBinaryOp E.greaterOrEqual (>=))
  , (E.lessOrEqual   , lispBinaryOp E.lessOrEqual (<=))
  , (E.notEqual      , lispBinaryOp E.notEqual (/=))
  , (E.whatIf        , lispIf)
  ]

builtins = M.fromList builtinList

builtinNames = map fst builtinList

-- -- -- Evaluators -- -- --

showAST :: AST -> Text
showAST (Sym x   ) = x
showAST (I32 x   ) = T.pack $ show x
showAST (Boo bool) = T.pack $ map toLower $ show bool
showAST Nul        = "null"
showAST (Nod x []) = T.concat ["(", showAST x, ")"]
showAST (Nod x xs) =
  T.concat ["(", showAST x, " ", T.unwords (map showAST xs), ")"]
showAST (Lst xs) = T.concat ["(list ", T.unwords (map showAST xs), ")"]
showAST (Err x ) = E.err <> " " <> x

evalLisp :: Text -> Either ParseError AST
evalLisp = fmap eval . parseLisp

evalLispToString :: Text -> Text
evalLispToString = either (T.pack . show) showAST . evalLisp
