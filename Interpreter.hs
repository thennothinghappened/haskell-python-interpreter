{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Interpreter(Env, Value, run, defaultEnvironment) where

import qualified Parser(Func(..), FuncArg(..), Stmt(..), Expr(..))
import Data.HashMap.Strict (HashMap, empty, insert, findWithDefault)
import Data.Maybe (fromMaybe)

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
call :: Env -> Parser.Func -> [Value] -> (Value, Env)
call env Parser.Func { args, body } passedArgs = run (createCallEnv env args passedArgs) body

-- |Initialise a sub-environment for a function call with the provided argument names and values.
createCallEnv :: Env -> [Parser.FuncArg] -> [Value] -> Env
createCallEnv env [] _ = env

-- Set the next function argument to the next passed value.
createCallEnv env (Parser.FuncArg nextArgName _ : restArgs) (nextArgValue : restArgValues) =
  let env' = env { vars = insert nextArgName nextArgValue env.vars }
  in createCallEnv env' restArgs restArgValues

-- Ran out of passed arguments, use the default.
createCallEnv env (Parser.FuncArg nextArgName nextArgExpr : restArgs) [] =
  let (nextArgValue, env') = eval env nextArgExpr
   in let env'' = env { vars = insert nextArgName nextArgValue env'.vars }
       in createCallEnv env'' restArgs []

-- |Execute a program retrieve the returned value, and the new program state.
run :: Env -> [Parser.Stmt] -> (Value, Env)
run env [] = (None, env)
run env (stmt : rest) = case runStmt env stmt of
  (None, env') -> run env' rest
  result -> result

-- |Execute a single statement in the given program environment.
runStmt :: Env -> Parser.Stmt -> (Value, Env)

runStmt env Parser.Assign { name, expr } =
  let (value, env') = eval env expr
  in (None, env { vars = insert name value env'.vars })

runStmt env (Parser.Return expr) = eval env expr

runStmt _ stmt = error ("Unhandled statement type " ++ show stmt)

-- |Evaluate the given expression, returning the result and updated program state.
eval :: Env -> Parser.Expr -> (Value, Env)
eval env (Parser.IntLit value) = (Int value, env)
eval env (Parser.Ref name) = (findWithDefault None name env.vars, env)
eval env (Parser.Add left right) =
  let (leftValue, env') = eval env left
   in let (rightValue, env'') = eval env' right
       in case add leftValue rightValue of
            Just result -> (result, env'')
            Nothing -> error ("Can't add " ++ show leftValue ++ " to " ++ show rightValue)
eval env Parser.None = (None, env)

-- |Add two runtime values together, if possible.
add :: Value -> Value -> Maybe Value
add (Int left) (Int right) = Just (Int (left + right))
add _ _ = Nothing
