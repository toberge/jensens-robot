module Lisp.Types where

data AST = I32 Int
         | Sym String
         | Nul
         | Err String
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)
