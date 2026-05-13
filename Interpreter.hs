{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Interpreter(Env, Value, run, defaultEnvironment) where

import qualified Parser(Func(..), FuncArg(..), Stmt(..), Expr(..))
import Data.HashMap.Strict (HashMap, empty, insert, findWithDefault)
import Control.Monad.State (State, MonadState (get), modify)

-- |A sandboxed environment in which to execute a program.
--  Represents the current program state.
newtype Env = Env {
  vars :: HashMap String Value
}

-- |An empty environment to begin execution of a program within.
defaultEnvironment :: Env
defaultEnvironment = Env empty

-- |A value of a given type at runtime.
data Value
  = Int Int
  | None
  deriving (Show)

-- |Call a function in the program with the provided arguments, and retrieve the returned value, and
--  the new program state.
call :: Parser.Func -> [Value] -> State Env Value
call Parser.Func { args, body } passedArgs = do
  createCallEnv args passedArgs
  run body

-- |Initialise a sub-environment for a function call with the provided argument names and values.
createCallEnv :: [Parser.FuncArg] -> [Value] -> State Env ()
createCallEnv [] _ = pure ()

-- Set the next function argument to the next passed value.
createCallEnv (Parser.FuncArg nextArgName _ : restArgs) (nextArgValue : restArgValues) = do
  modify (\env -> env { vars = insert nextArgName nextArgValue env.vars })
  createCallEnv restArgs restArgValues

-- Ran out of passed arguments, use the default.
createCallEnv (Parser.FuncArg nextArgName nextArgExpr : restArgs) [] = do
  nextArgValue <- eval nextArgExpr
  modify (\env -> env { vars = insert nextArgName nextArgValue env.vars })
  createCallEnv restArgs []

-- |Execute a program retrieve the returned value, and the new program state.
run :: [Parser.Stmt] -> State Env Value
run [] = pure None
run (stmt : rest) = do
  result <- runStmt stmt

  case result of
    None -> run rest
    result -> pure result

-- |Execute a single statement in the given program environment.
runStmt :: Parser.Stmt -> State Env Value
runStmt Parser.Assign { name, expr } = do
  value <- eval expr
  modify (\env -> env { vars = insert name value env.vars })
  pure None

runStmt (Parser.Return expr) = eval expr
runStmt stmt = error ("Unhandled statement type " ++ show stmt)

-- |Evaluate the given expression, returning the result and updated program state.
eval :: Parser.Expr -> State Env Value
eval (Parser.IntLit value) = pure (Int value)

eval (Parser.Ref name) = do
  env <- get
  pure (findWithDefault None name env.vars)

eval (Parser.Add left right) = do
  leftValue <- eval left
  rightValue <- eval right
  
  case add leftValue rightValue of
      Just result -> pure result
      Nothing -> error ("Can't add " ++ show leftValue ++ " to " ++ show rightValue)

eval Parser.None = pure None

-- |Add two runtime values together, if possible.
add :: Value -> Value -> Maybe Value
add (Int left) (Int right) = Just (Int (left + right))
add _ _ = Nothing
