module Lisp.Types where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

data AST = I32 Int
         | Sym Text
         | Nul
         | Err Text
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         | Fun [Text] AST
         deriving (Eq, Show)

type Scope = Map Text AST
type Builtin = LispState -> [AST] -> AST

data LispState = LispState
  { globalScope   :: Scope
  , localScope    :: Scope
  , stateBuiltins :: Map Text Builtin
  }
