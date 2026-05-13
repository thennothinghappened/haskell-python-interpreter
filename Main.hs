module Main where

import Prelude hiding (lex)
import Lexer(lexString)
import Parser(parse)
import Interpreter(run, defaultEnvironment)
import Control.Monad.State (runState)

main :: IO ()
main = do
  input <- readFile "input.py"
  
  let tokens = lexString input
  print tokens

  let program = parse tokens
  print program

  let (result, _) = runState (run program) defaultEnvironment
  print result

  pure ()
