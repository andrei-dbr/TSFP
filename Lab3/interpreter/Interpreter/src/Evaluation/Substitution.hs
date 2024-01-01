module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVars :: Expression -> [String]
freeVars = toList . freeVarsSet

freeVarsSet :: Expression -> Set ID
freeVarsSet (Var x) = singleton x
freeVarsSet (Lambda x e) = delete x $ freeVarsSet e
freeVarsSet (Application e1 e2) = union (freeVarsSet e1) (freeVarsSet e2)
freeVarsSet _ = empty

renameVarID :: ID -> ID
renameVarID = ('#':)

{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression

subst x e' e@(Var y) = if x == y then e' else e
subst x e' lambda@(Lambda y e) =
    if (x == y)
    then
        lambda
    else
        if (member y (freeVarsSet e'))
        then let z = (renameVarID y) in 
            (Lambda z (subst x e' (subst y (Var z) e)))
        else
            (Lambda y (subst x e' e))

subst x e' (Application e1 e2) = (Application (subst x e' e1) (subst x e' e2))