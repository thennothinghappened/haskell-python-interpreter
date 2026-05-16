{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Interpreter (Env, Value, Result(..), EvalResult, run, defaultEnvironment) where

import Parser (Func (Func), FuncArg (..), Stmt, Expr, BinOp)
import qualified Parser as Func (Func(..))
import qualified Parser as Stmt (Stmt(..))
import qualified Parser as Expr (Expr(..))
import qualified Parser as BinOp (BinOp(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State (State, MonadState (get), modify)
import Data.Functor ((<&>))
import Control.Applicative.Combinators ((<|>))

-- |A sandboxed environment in which to execute a program.
--  Represents the current program state.
data Env = Env {
  -- |The global scope in the environment.
  global :: Scope,

  -- |Newest-first stack of scopes for function calls.
  callStack :: [Scope]
}

data Scope = Scope {
  -- |Variables defined in this scope.
  vars :: Map String Value,

  -- |Set of variable names which should refer to variables in the global scope if assigned.
  globals :: Set String
}

-- |Retrieve a variable's value by its name in the environment, if it exists.
getVar :: String -> Env -> Maybe Value
getVar name env@Env { callStack = (scope : _), global }
  | treatAsGlobal name env = Map.lookup name global.vars
  | otherwise = Map.lookup name scope.vars <|> Map.lookup name global.vars
getVar name Env { global } = Map.lookup name global.vars

-- |Set a variable to the provided value.
setVar :: String -> Value -> State Env ()
setVar name value = do
  env <- get
  if treatAsGlobal name env
    then modifyGlobalScope $ scopeSetVar name value
    else modifyScope $ scopeSetVar name value

-- |Check whether the variable with the provided name is defined as referring to a global.
treatAsGlobal :: String -> Env -> Bool
treatAsGlobal name env = Set.member name (scope env).globals

-- |Make the given variable name refer to a global variable even when assigned.
makeGlobalReference :: String -> State Env ()
makeGlobalReference name =
  modifyScope (\scope -> scope { globals = Set.insert name scope.globals })

-- |Retrieve the current scope in this environment.
scope :: Env -> Scope
scope Env { callStack = (scope : _) } = scope
scope Env { global } = global

modifyScope :: (Scope -> Scope) -> State Env ()
modifyScope f = modify (\env -> case env of
    Env { callStack = (scope : rest) } -> env { callStack = f scope : rest }
    Env { global } -> env { global = f global }
  )

modifyGlobalScope :: (Scope -> Scope) -> State Env ()
modifyGlobalScope f = modify (\env -> env { global = f env.global })

-- |Set a variable by name in the scope to the given value.
scopeSetVar :: String -> Value -> Scope -> Scope
scopeSetVar name value scope = scope { vars = Map.insert name value scope.vars }

-- |An empty environment to begin execution of a program within.
defaultEnvironment :: Env
defaultEnvironment = Env emptyScope []

emptyScope :: Scope
emptyScope = Scope Map.empty Set.empty

-- |A value of a given type at runtime.
data Value
  = Int Int
  | String String
  | Bool Bool
  | FuncRef Func
  | None
  deriving (Show)

-- |Generic result type.
data Result t e
  = Ok t
  | Err e
  deriving (Show)

-- |The result of evaluating an expression at runtime.
type EvalResult = Result Value RuntimeError
type RuntimeError = String

-- |Call a function in the program with the provided arguments, and retrieve the returned value, and
--  the new program state.
call :: Func -> [Value] -> State Env EvalResult
call func@Func { args, body } passedArgs = do
  callArgsResult <- evalCallArgs args passedArgs

  case callArgsResult of
    Ok callArgs -> do
      modify $ pushCallScope callArgs
      result <- run body
      modify popCallScope
      pure result
    Err message -> pure $ Err $ "Error whilst calling " ++ show func ++ ": " ++ message

evalCallArgs :: [FuncArg] -> [Value] -> State Env (Result [(String, Value)] RuntimeError)
evalCallArgs [] _ =
  pure $ Ok []

evalCallArgs (FuncArg name Nothing : _) [] =
  pure $ Err $ "No value provided for non-optional argument " ++ name

evalCallArgs (FuncArg name (Just expr) : restArgs) [] = do
  valueResult <- eval expr

  case valueResult of
    Ok value -> do
      restResult <- evalCallArgs restArgs []

      case restResult of
        Ok restArgPairs -> pure $ Ok ((name, value) : restArgPairs)
        Err message -> pure $ Err message

    Err message -> pure $ Err ("Error in evaluating the default value for argument " ++ name ++ ": " ++ message)

evalCallArgs (arg : restArgs) (value : restValues) = do
  restResult <- evalCallArgs restArgs restValues
  case restResult of
    Ok restArgPairs -> pure $ Ok ((arg.name, value) : restArgPairs)
    Err message -> pure $ Err message

-- |Initialise a sub-environment for a function call with the provided argument names and values.
pushCallScope :: [(String, Value)] -> Env -> Env
pushCallScope args env = env { callStack = Scope (Map.fromList args) Set.empty : env.callStack }

-- |Leave the current function and return to the previous scope.
popCallScope :: Env -> Env
popCallScope env@Env { callStack = (_ : rest) } = env { callStack = rest }
popCallScope _ = undefined

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
      setVar name value
      pure Nothing
    Err message -> pure $ Just (Err message)

runStmt Stmt.DefineFunc { name, func } = do
  setVar name (FuncRef func)
  pure Nothing

runStmt (Stmt.Return expr) = eval expr <&> Just
runStmt (Stmt.Global name) = makeGlobalReference name >> pure Nothing

runStmt (Stmt.PoisonStmt { span, message }) =
  pure (Just (Err ("Malformed program at " ++ show span ++ ": " ++ message)))

-- |Evaluate the given expression, returning the result and updated program state.
eval :: Expr -> State Env EvalResult
eval (Expr.IntLit value) = pure (Ok (Int value))
eval (Expr.StringLit value) = pure (Ok (String value))
eval (Expr.BoolLit value) = pure (Ok (Bool value))

eval (Expr.Ref name) = do
  env <- get

  case getVar name env of
    Just value -> pure $ Ok value
    Nothing -> pure $ Err $ "Reference to undefined variable " ++ show name

eval (Expr.BinOp op left right) = evalBinOp op left right

eval (Expr.Call target args) = do
  targetValue <- eval target

  case targetValue of
    Ok (FuncRef func) -> do
      argValuesResult <- evalArgs args

      case argValuesResult of
        Ok argValues -> call func argValues
        Err message -> pure $ Err message

    Ok value -> pure $ Err ("Value " ++ show value ++ " is not callable")
    Err message -> pure $ Err message
  where
    evalArgs :: [Expr] -> State Env (Result [Value] RuntimeError)
    evalArgs [] = pure $ Ok []
    evalArgs (arg : rest) = do
      result <- eval arg

      case result of
        Ok value -> do
          restResult <- evalArgs rest

          case restResult of
            Ok restValues -> pure $ Ok (value : restValues)
            Err message -> pure $ Err message

        Err message -> pure $ Err message

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
