{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Interpreter (Env, Value, run, defaultEnvironment) where

import Parser (Func, FuncArg, Stmt, Expr, BinOp)
import qualified Parser as Func (Func(..))
import qualified Parser as FuncArg (FuncArg(..))
import qualified Parser as Stmt (Stmt(..))
import qualified Parser as Expr (Expr(..))
import qualified Parser as BinOp (BinOp(..))

import Control.Monad.State (State, MonadState (get), modify)
import qualified Data.Map as Map
import Data.Functor ((<&>))

-- |A sandboxed environment in which to execute a program.
--  Represents the current program state.
newtype Env = Env {
  vars :: Map.Map String Value
}

-- |An empty environment to begin execution of a program within.
defaultEnvironment :: Env
defaultEnvironment = Env Map.empty

-- |A value of a given type at runtime.
data Value
  = Int Int
  | String String
  | None
  deriving (Show)

-- |The result of evaluating an expression at runtime.
data EvalResult
  = Ok Value
  | Err String
  deriving (Show)

-- -- |Call a function in the program with the provided arguments, and retrieve the returned value, and
-- --  the new program state.
-- call :: Parser.Func -> [Value] -> State Env EvalResult
-- call Parser.Func { args, body } passedArgs = do
--   createCallEnv args passedArgs
--   run body

-- -- |Initialise a sub-environment for a function call with the provided argument names and values.
-- createCallEnv :: [Parser.FuncArg] -> [Value] -> State Env ()
-- createCallEnv [] _ = pure ()

-- -- Set the next function argument to the next passed value.
-- createCallEnv (Parser.FuncArg nextArgName _ : restArgs) (nextArgValue : restArgValues) = do
--   modify (\env -> env { vars = Map.insert nextArgName nextArgValue env.vars })
--   createCallEnv restArgs restArgValues

-- -- Ran out of passed arguments, use the default.
-- createCallEnv (Parser.FuncArg nextArgName nextArgExpr : restArgs) [] = do
--   nextArgValue <- eval nextArgExpr
--   modify (\env -> env { vars = Map.insert nextArgName nextArgValue env.vars })
--   createCallEnv restArgs []

-- |Execute a program retrieve the returned value, and the new program state.
run :: [Stmt] -> State Env EvalResult
run [] = pure $ Ok None
run (stmt : rest) = do
  result <- runStmt stmt

  case result of
    Nothing -> run rest
    Just result -> pure result

-- |Execute a single statement in the given program environment.
runStmt :: Stmt -> State Env (Maybe EvalResult)
runStmt Stmt.Assign { name, expr } = do
  exprResult <- eval expr

  case exprResult of
    Ok value -> do
      modify (\env -> env { vars = Map.insert name value env.vars })
      pure Nothing
    Err message -> pure $ Just (Err message)

runStmt (Stmt.Return expr) = eval expr <&> Just

runStmt (Stmt.PoisonStmt { span, message }) =
  pure (Just (Err ("Malformed program at " ++ show span ++ ": " ++ message)))

-- |Evaluate the given expression, returning the result and updated program state.
eval :: Expr -> State Env EvalResult
eval (Expr.IntLit value) = pure (Ok (Int value))
eval (Expr.StringLit value) = pure (Ok (String value))

eval (Expr.Ref name) = do
  env <- get

  case Map.lookup name env.vars of
    Just value -> pure $ Ok value
    Nothing -> pure $ Err $ "Reference to undefined variable " ++ show name

eval (Expr.BinOp op left right) = evalBinOp op left right
eval Expr.None = pure (Ok None)

-- |Evaluate the result of the given binary operation between two values.
evalBinOp :: BinOp -> Expr -> Expr -> State Env EvalResult
evalBinOp op left right = do
  leftResult <- eval left

  case leftResult of
    Ok leftValue -> evalUnaryOp (performBinOp op leftValue) right
    Err message -> pure (Err message)

-- |Evaluate the result of the given unary operation on the given value.
evalUnaryOp :: (Value -> EvalResult) -> Expr -> State Env EvalResult
evalUnaryOp f expr = do
  valueResult <- eval expr

  case valueResult of
    Ok value -> pure (f value)
    Err message -> pure (Err message)

-- |Perform the given binary operation between two values.
performBinOp :: BinOp -> Value -> Value -> EvalResult
performBinOp BinOp.Add (Int left) (Int right) = Ok $ Int (left + right)
performBinOp BinOp.Add (String left) (String right) = Ok $ String (left ++ right)
performBinOp BinOp.Sub (Int left) (Int right) = Ok $ Int (left - right)
performBinOp op left right = Err ("Can't perform op " ++ show left ++ show op ++ show right)
