module Syntax.Expression where

import Data.Map

type ID = String

data Expression
    = Var ID
    | Lambda ID Expression
    | Application Expression Expression
    | Definition ID Expression
    deriving (Show, Eq)

type Context = Map ID Expression