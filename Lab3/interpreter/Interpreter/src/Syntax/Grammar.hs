module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser

import Control.Applicative

import Data.Char

parseProgram :: String -> Maybe [Expression]
parseProgram = parse exprList
  where
    exprList :: Parser [Expression]
    exprList = (some ((def <|> expr) <* many (spot isSpace))) <* eof 
