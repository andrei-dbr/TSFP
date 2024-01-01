module Evaluation.Big where

import Syntax.Expression
import Data.List
import qualified Data.Map as M
import Data.Tuple (swap)
import Evaluation.Normal

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition
evalBig eval expr context
    | newExpr == expr = (newExpr, newContext)
    | otherwise = evalBig eval newExpr newContext
    where
        (newExpr, newContext) = eval expr context

evalBigM :: (Expression -> Eval Expression)
         -> Expression
         -> Eval Expression

evalBigM evalM expr = do
    newExpr <- evalM expr
    if newExpr == expr
    then
        return newExpr
    else
        evalBigM evalM newExpr

{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList eval exprs context = swap $ mapAccumL (\ctx e -> swap (evalBig eval e ctx)) M.empty exprs


evalListM :: (Expression -> Eval Expression)
          -> [Expression]
          -> Eval [Expression]  

evalListM = mapM . evalBigM
