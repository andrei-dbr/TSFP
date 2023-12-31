module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

{-|
    Small-step applicative-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval (Definition id expr) context = (expr, M.insert id expr context)

eval (Var id) context = (context M.! id, context)

eval lambda@(Lambda _ _) context = (lambda, context)

eval (Application (Lambda id e) e'@(Lambda _ _)) context = (subst id e' e, context)

eval (Application lambda@(Lambda _ _) e) context =
    (Application lambda newE, newContext)
        where
            (newE, newContext) = eval e context

eval (Application e1 e2) context = ((Application (fst $ eval e1 context) e2), context)
