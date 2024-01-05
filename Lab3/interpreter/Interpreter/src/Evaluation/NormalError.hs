module Evaluation.NormalError where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Except
 
type Eval = StateT Context (ExceptT String IO)

evalM :: Expression -> Eval Expression

evalM (Definition id expr) = do
    modify $ M.insert id expr
    return expr

evalM (Var id) = do
    ctx <- get
    case M.lookup id ctx of
        Nothing -> throwError $ "Unknown variable " ++ id
        Just e -> return e

evalM (Lambda x e) = return $ Lambda x e

evalM (Application (Lambda id e) e') = return $ subst id e' e

evalM (Application e1 e2) =
    liftM2 Application (evalM e1) (return e2)

runEval :: Eval a -> IO ()
runEval evalAction = do
  result <- runExceptT (runStateT evalAction M.empty)
  case result of
    Right (_, finalContext) ->
      putStrLn $ "Final Context: " ++ show finalContext
    Left err -> putStrLn $ "Error: " ++ err