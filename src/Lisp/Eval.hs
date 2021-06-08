module Lisp.Eval
  ( showAST
  , evalLisp
  , evalLispToString
  , builtinNames
  ) where

import           Control.Applicative
import           Data.Char                      ( toLower )
import           Data.List                      ( find )
import qualified Data.Map                      as M
import           Data.Maybe
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

eval :: LispState -> AST -> AST
eval s (Seq []            ) = Seq []
-- TODO decide if all or only last in Seq should be the result! (important for possible use in function body...)
eval s (Seq (node : nodes)) = case eval s node of
  Sco globals -> eval s { globalScope = globals } (Seq nodes)
  err@Err{}   -> err
  whatever    -> Seq $ whatever : fromSeq (eval s (Seq nodes))
eval s@LispState { localScope = locals, globalScope = globals } (Nod (Sym sym) args)
  | sym `M.member` builtins -- Builtins are called directly
  = maybe (Err $ sym <> " er ingen kjent funksjon") (\f -> f s args)
    $ M.lookup sym builtins
  | otherwise
  -- User-defined functions must be evaluated themselves
  = maybe (Err $ sym <> " er ingen kjent funksjon")
          (evalFunctionCall s args)
          (M.lookup sym locals <|> M.lookup sym globals)
eval s (Nod fun@Fun{}  args) = evalFunctionCall s args fun
eval s (Nod node@Nod{} args) = case eval s node of
  fun@Fun{} -> evalFunctionCall s args fun
  _         -> Err "Invalid function call"
eval s (Lst values) = Lst $ map (eval s) values
eval s@LispState { localScope = locals, globalScope = globals } (Sym sym) =
  fromMaybe (Err $ sym <> " er ikke en variabel")
            (M.lookup sym locals <|> M.lookup sym globals)
eval _ value = value

fromSeq :: AST -> [AST]
fromSeq (Seq nodes) = nodes
fromSeq _           = error "Invalid use of fromSeq"

-- TODO this should not be called when the symbol is a value
evalFunctionCall :: LispState -> [AST] -> AST -> AST
evalFunctionCall s@LispState { localScope = locals } args (Fun argNames body)
  | length args == length argNames = eval s { localScope = mergedScope } body
  | otherwise = Err "Arguments given do not match arguments required"
 where
  mergedScope = M.union argMap locals
  argMap      = M.fromList $ zipWith (\k v -> (k, eval s v)) argNames args
evalFunctionCall _ _ err@Err{} = err
evalFunctionCall _ _ _ =
  Err "Unknown error in function call – is this a function?"

lispLambda _ [Nod argHead argTail, body]
  | all isSymbol args = Fun (map getSymbol args) body
  | otherwise         = Err $ E.defn <> ": Not all args in arg list are args"
  where args = argHead : argTail
lispLambda _ _ = Err $ "Wrong way to call " <> E.lambda

lispDefineFunction s@LispState { globalScope = globals } [Sym name, Nod argHead argTail, body]
  | all isSymbol args
  = Sco $ M.insert name fun globals
  | otherwise
  = Err $ E.defn <> ": Not all args in arg list are args"
 where
  args = argHead : argTail
  fun  = Fun (map getSymbol args) body
lispDefineFunction _ _ = Err $ "Wrong way to call " <> E.defn

lispSetVariable s@LispState { globalScope = globals } [Sym name, value] =
  Sco $ M.insert name value globals
lispSetVariable _ _ = Err $ "Wrong way to call " <> E.setq

isSymbol :: AST -> Bool
isSymbol (Sym x) = True
isSymbol _       = False

getSymbol :: AST -> Text
getSymbol (Sym x) = x
getSymbol _       = error "Use isSymbol before getSymbol!"

-- Wrapper for *boolean* ops
lispBinaryOp name op = fun
 where
  fun :: Builtin
  fun _ [I32 a, I32 b] = Boo $ op a b
  fun _ [Err x, _    ] = Err x
  fun _ [_    , Err x] = Err x
  fun s [a    , b    ] = fun s [eval s a, eval s b]
  fun _ _              = Err $ name <> " trenger to tall"

-- Wrapper for binary ops, but generic
lispBinaryOpGeneric name op = fun
 where
  fun :: Builtin
  fun _ [I32 a, I32 b] = op a b
  fun _ [Err x, _    ] = Err x
  fun _ [_    , Err x] = Err x
  fun s [a    , b    ] = fun s [eval s a, eval s b]
  fun _ _              = Err $ name <> " trenger to tall"

isI32 (I32 _) = True
isI32 _       = False

isList (Lst _) = True
isList _       = False

isValid (Err _) = False
isValid _       = True

getList (Lst xs) = xs
getList _        = [Err "wtf"]

-- Wrapper for binary functions that should work on multiple values
lispMultiOp :: Text -> (Int -> Int -> Int) -> Builtin
lispMultiOp name op = fun
 where
  fun _ [] = Err $ name <> " trenger argumenter"
  fun s xs' | all isI32 xs = I32 $ foldl1 op $ map (\(I32 x) -> x) xs
            | otherwise    = Err $ name <> " kan bare motta tall som argumenter"
    where xs = map (eval s) xs'

lispNot s [Boo bool] = Boo $ not bool
lispNot s [ast     ] = case eval s ast of
  (Boo bool) -> Boo $ not bool
  _          -> Err $ E.nope <> " kan bare brukes på boolske verdier"
lispNot _ _ = Err $ E.nope <> " trenger ett argument"

lispList s = Lst . map (eval s)

lispSize s [node] = case eval s node of
  (Lst xs) -> I32 $ length xs
  _        -> Err "Kan ikke finne størrelsen til noe som ikke er ei liste"
lispSize _ _ = Err "Kan bare finne størrelsen til én ting"

lispReverse s [node] = case eval s node of
  (Lst xs) -> Lst $ reverse xs
  _        -> Err "Kan ikke reversere noe som ikke er ei liste"
lispReverse _ _ = Err "Kan bare reversere ei liste, verken mer eller mindre"

lispMember s [x', list']
  | isList list && isValid x = Boo $ elem x $ getList list
  | otherwise = Err $ E.member <> " kan bare brukes på element og liste"
 where
  list = eval s list'
  x    = eval s x'


lispRange = lispBinaryOpGeneric ".." range
  where range a b = Lst (map I32 [a .. b])

lispPower = lispBinaryOpGeneric "^" pow where pow a b = I32 $ a ^ b

lispIf :: Builtin
lispIf s [Boo x, a]    = if x then eval s a else Nul
lispIf s [Boo x, a, b] = if x then eval s a else eval s b
lispIf s (x : xs)      = case res of
  bool@(Boo _) -> lispIf s (bool : xs)
  _            -> Err $ E.whatIf <> " krever boolsk verdi som første argument"
  where res = eval s x
lispIf _ _ = Err $ "Ugyldig argument til " <> E.whatIf

builtinList :: [(Text, Builtin)]
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
  , (E.lambda        , lispLambda)
  , (E.defn          , lispDefineFunction)
  , (E.setq          , lispSetVariable)
  ]

builtins = M.fromList builtinList

builtinNames = map fst builtinList

-- -- -- Evaluators -- -- --

showAST :: AST -> Text
showAST (Sco globals) = T.pack $ show globals
showAST (Seq xs     ) = T.unlines $ map showAST xs
showAST (Fun _ _    ) = "(function)"
showAST (Sym x      ) = x
showAST (I32 x      ) = T.pack $ show x
showAST (Boo bool   ) = T.pack $ map toLower $ show bool
showAST Nul           = "null"
showAST (Nod x [])    = T.concat ["(", showAST x, ")"]
showAST (Nod x xs) =
  T.concat ["(", showAST x, " ", T.unwords (map showAST xs), ")"]
showAST (Lst xs) = T.concat ["(list ", T.unwords (map showAST xs), ")"]
showAST (Err x ) = E.err <> " " <> x

evalLisp :: Text -> Either ParseError AST
evalLisp = fmap (eval initialState) . parseLisp
 where
  initialState = LispState { stateBuiltins = builtins
                           , globalScope   = M.empty
                           , localScope    = M.empty
                           }

evalLispToString :: Text -> Text
evalLispToString = either (T.pack . show) showAST . evalLisp
