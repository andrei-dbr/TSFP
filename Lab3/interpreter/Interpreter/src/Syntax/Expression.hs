module Syntax.Expression where

data ID = ID String deriving Show

data Expression
    = Var ID
    | Lambda ID Expression
    | Application Expression Expression
    | Definition ID Expression
    deriving Show
