{-# LANGUAGE ViewPatterns #-}

import Prelude (String, Int, IO, readFile, putStrLn, Monad ((>>=)), Show (show), pure, mapM_, print, Maybe (Just, Nothing), Eq ((==)), Char)
import Data.List
import Data.Bool
import Data.Char (isAlphaNum, isAlpha, isNumber)
import Text.Read (readMaybe)

data Token =
    Ident String
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
lex (' ' : rest) = lex rest
lex ('\n' : rest) = lex rest
lex ('\t' : rest) = lex rest
lex input
  | isAlpha (head input) || (head input == '_') =
    let (ident, rest) = consumeIdent input in
      Ident ident : lex rest
  | isNumber (head input) = let (value, rest) = consumeInt input in
      IntLit value : lex rest
  | otherwise = []

consumeIdent :: String -> (String, String)
consumeIdent input
  | isAlphaNum (head input) || (head input == '_') = span isValidIdent input
  | otherwise = ([], input)

consumeInt :: String -> (Int, String)
consumeInt input = let (numString, rest) = span isNumber input in
  case readMaybe numString of
    Just value -> (value, rest)
    Nothing -> (0, rest)

isValidIdent :: Char -> Bool
isValidIdent c = isAlphaNum c || (c == '_')
