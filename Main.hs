module Main where

import Lexer (lexString)
import Parser (parse)
import Interpreter (Result(..), run, defaultEnvironment)
import Control.Monad.State (evalState)

main :: IO ()
main = do
  input <- readFile "input.py"

  let tokens = lexString input
  putStrLn "\n==== Tokens ===="
  print tokens

  let program = parse tokens

  putStrLn "\n==== Parsed Code ===="
  -- i have no idea what im doing!
  putStrLn $ concatMap ((++ "\n") . show) program

  putStrLn "\n==== Interpreter Output ===="
  case evalState (run program) defaultEnvironment of
    Ok value -> putStrLn $ "Program returned: " ++ show value
    Err message -> putStrLn $ "Program failed with message: " ++ show message

  pure ()
