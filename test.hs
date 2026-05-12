{-# LANGUAGE ViewPatterns #-}

import Prelude hiding (lex)
import Data.Char (isAlphaNum, isAlpha, isNumber)
import Text.Read (readMaybe)
import Unicode.Char (isWhiteSpace)

data Token
  = Ident String
  | IntLit Int
  | SingleEquals
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.py"
  print (lex input)

lex :: String -> [Token]
lex [] = []
lex ('=' : rest) = SingleEquals : lex rest
lex input
  | isValidIdentStart c = lexIdent input
  | isNumber c = lexInt input
  | isWhiteSpace c = lex (dropWhile isWhiteSpace input)
  | otherwise = []
  where
    c = head input

lexIdent :: String -> [Token]
lexIdent input = Ident ident : lex rest
  where (ident, rest) = consumeIdent input

lexInt :: String -> [Token]
lexInt input = IntLit value : lex rest
  where (value, rest) = consumeInt input

consumeIdent :: String -> (String, String)
consumeIdent input
  | isAlphaNum c || (c == '_') = span isValidIdent input
  | otherwise = ([], input)
  where
    c = head input

consumeInt :: String -> (Int, String)
consumeInt input =
  let (numString, rest) = span isNumber input
    in case readMaybe numString of
      Just value -> (value, rest)
      Nothing -> undefined

isValidIdent :: Char -> Bool
isValidIdent c = isAlphaNum c || (c == '_')

isValidIdentStart :: Char -> Bool
isValidIdentStart c = isAlpha c || (c == '_')
