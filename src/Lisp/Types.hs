module Lisp.Types where

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
