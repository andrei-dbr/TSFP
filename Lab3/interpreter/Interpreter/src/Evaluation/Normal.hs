module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

import Control.Monad.State
 
type Eval = State Context

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takMes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition

eval (Definition id expr) context = (expr, M.insert id expr context)

eval (Var id) context = (context M.! id, context)

eval lambda@(Lambda _ _) context = (lambda, context)

eval (Application (Lambda id e) e') context = (subst id e' e, context)

eval (Application e1 e2) context = ((Application (fst $ eval e1 context) e2), context)


evalM :: Expression -> Eval Expression

evalM (Definition id expr) = do
    modify $ M.insert id expr
    return expr

evalM (Var id) = do
    ctx <- get
    return $ ctx M.! id

evalM (Lambda x e) = return $ Lambda x e

evalM (Application (Lambda id e) e') = return $ subst id e' e

evalM (Application e1 e2) =
    liftM2 Application (evalM e1) (return e2)

