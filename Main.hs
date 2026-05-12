module Main where

import Prelude hiding (lex)
import Lexer(lexString)

main :: IO ()
main = do
  input <- readFile "input.py"
  print (lexString input)
