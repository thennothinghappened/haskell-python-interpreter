{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Main where

import Lexer (lexString)
import Parser (parse)
import Interpreter (Env(..), Result(..), run, defaultEnvironment)
import Control.Monad.State (evalState, runState)

main :: IO ()
main = do
  input <- readFile "input.py"

  let tokens = lexString input
  putStrLn "\n==== Tokens ===="
  print tokens

  let program = parse tokens

  putStrLn "\n==== Parsed Code ===="
  putStrLn $ unlines (map show program)

  putStrLn "\n==== Interpreter Output ===="
  let (result, env) = runState (run program) defaultEnvironment

  putStrLn env.stdout
  case result of
    Just (Ok value) -> putStrLn $ "Program returned: " ++ show value
    Just (Err message) -> putStrLn $ "Program failed with message: " ++ show message
    Nothing -> putStrLn "Program exited with no return value"

  pure ()
