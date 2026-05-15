{-# LANGUAGE ViewPatterns, OverloadedRecordDot, DuplicateRecordFields #-}

module Lexer(Token(..), TokenInfo(..), Location, Span, lexString) where

import Prelude hiding (lex)
import Data.Char (isAlphaNum, isAlpha)
import Text.Read (readMaybe)
import Unicode.Char (isWhiteSpace)

-- |Convert the passed string into a sequence of lexical tokens that can be later parsed.
lexString :: String -> [TokenInfo]
lexString inputString = lex (Input inputString startOfFile)

data Token
  = Ident String
  | IntLit Int
  | StringLit String
  | SingleEquals
  | Plus
  | Minus
  | OpenParen
  | CloseParen
  | Colon
  | Comma
  | Def
  | Return
  | Global
  | None
  | NewLine { indents :: Int }
  deriving (Show)

data Location = Location {
  row :: Int,
  column :: Int
} deriving(Eq)

nextColumn :: Location -> Location
nextColumn Location { row, column } = Location { row, column = column + 1 }

nextRow :: Location -> Location
nextRow Location { row } = Location { row = row + 1, column = 1 }

instance Show Location where
  show Location { row, column } = show row ++ ":" ++ show column

instance Ord Location where
  compare :: Location -> Location -> Ordering
  compare a b
    | a.row < b.row = LT
    | a.row > b.row = GT
    | a.column > b.column = GT
    | a.column < b.column = LT
    | a.column == b.column = EQ
    | otherwise = EQ

startOfFile :: Location
startOfFile = Location { row = 1, column = 1 }

data Span = Span {
  start :: Location,
  end :: Location
}

emptySpan :: Location -> Span
emptySpan location = Span location location

union :: Span -> Span -> Span
a `union` b = Span (min a.start b.start) (max a.end b.end)

instance Show Span where
  show Span { start, end } = show start ++ " to " ++ show end

data TokenInfo = TokenInfo Token Span

instance Show TokenInfo where
  show (TokenInfo token span) = show token ++ " from " ++ show span

data Input = Input {
  text :: String,
  start :: Location
}

-- |Move to the next character of the input.
inputNext :: Input -> Input
inputNext input =
  case inputMapNext Just input of
    Just (_, input') -> input'
    Nothing -> input

-- |Map the next character using the given function, returning the mapped value if successful.
inputMapNext :: (Char -> Maybe a) -> Input -> Maybe ((a, Span), Input)
inputMapNext _ (Input [] _) = Nothing
inputMapNext f (Input (c : rest) start) = do
  value <- f c
  let end = updateLocation start c
  Just ((value, Span start end), Input rest end)

-- |Accept a single character matching the predicate.
inputAccept :: (Char -> Bool) -> Input -> Maybe ((Char, Span), Input)
inputAccept f = inputMapNext (\c -> if f c then Just c else Nothing)

-- |Consume the given character from the start of the input, if present.
inputConsume :: Char -> Input -> Maybe (Span, Input)
inputConsume c (inputAccept (== c) -> Just ((_, span), input)) = Just (span, input)
inputConsume _ _ = Nothing

inputSpan :: (Char -> Bool) -> Input -> Maybe ((String, Span), Input)
inputSpan f (inputAccept f -> Just ((c, start), input)) =
  case inputSpan f input of
    Just ((match, end), input') -> Just ((c : match, start `union` end), input')
    Nothing -> Just (([c], start), input)
inputSpan _ _ = Nothing

-- |Remove the given prefix from the input if present, returning the rest of the input if
--  successful, and the span from start to end of the prefix.
inputStripPrefix :: String -> Input -> Maybe (Span, Input)
inputStripPrefix [] input = Just (emptySpan input.start, input)
inputStripPrefix (c : restPrefix) (inputConsume c -> Just (startSpan, input)) = do
  (Span _ end, input') <- inputStripPrefix restPrefix input
  Just (Span startSpan.start end, input')
inputStripPrefix _ _ = Nothing

-- |Match the given prefix with the input, and convert it to an instance of the token if successful.
inputToken :: String -> Token -> Input -> Maybe (TokenInfo, Input)
inputToken text token input = do
  (span, input') <- inputStripPrefix text input
  Just (TokenInfo token span, input')

-- |Increment the row or column of the given location as appropriate to the character there.
updateLocation :: Location -> Char -> Location
updateLocation location '\r' = location
updateLocation location '\n' = nextRow location
updateLocation location _ = nextColumn location

lex :: Input -> [TokenInfo]
lex (Input [] _) = []
lex (inputToken "def" Def -> Just (token, input)) = token : lex input
lex (inputToken "return" Return -> Just (token, input)) = token : lex input
lex (inputToken "global" Global -> Just (token, input)) = token : lex input
lex (inputToken "None" None -> Just (token, input)) = token : lex input
lex (inputToken "=" SingleEquals -> Just (token, input)) = token : lex input
lex (inputToken "+" Plus -> Just (token, input)) = token : lex input
lex (inputToken "-" Minus -> Just (token, input)) = token : lex input
lex (inputToken "(" OpenParen -> Just (token, input)) = token : lex input
lex (inputToken ")" CloseParen -> Just (token, input)) = token : lex input
lex (inputToken ":" Colon -> Just (token, input)) = token : lex input
lex (inputToken "," Comma -> Just (token, input)) = token : lex input

lex (inputConsume '\n' -> Just (start, input)) =
    case lexIndents input of
      Just ((indents, end), input') -> coalesceLines indents end input'
      Nothing -> coalesceLines 0 start input
  where
    coalesceLines :: Int -> Span -> Input -> [TokenInfo]
    coalesceLines indents end input =
      case lex input of
        (TokenInfo (NewLine indents') end' : rest) ->
          TokenInfo (NewLine indents') (start `union` end')
          : rest
        
        rest ->
          TokenInfo (NewLine indents) (start `union` end)
          : rest

lex (inputSpan isWhiteSpace -> Just (_, input)) = lex input
lex (lexNumber -> Just ((num, span), input)) = TokenInfo (IntLit num) span : lex input
lex (lexIdent -> Just ((ident, span), input)) = TokenInfo (Ident ident) span : lex input
lex (lexStringLit -> Just ((string, span), input)) = TokenInfo (StringLit string) span : lex input
lex (Input (c : _) start) = error ("Unexpected character " ++ show c ++ " in input at " ++ show start)

lexNumber :: Input -> Maybe ((Int, Span), Input)
lexNumber (inputMapNext (\c -> readMaybe [c]) -> Just ((digit, start), input)) =
  case lexNumber input of
    Just ((nextDigit, end), input') -> Just ((digit * 10 + nextDigit, start `union` end), input')
    Nothing -> Just ((digit, start), input)
lexNumber _ = Nothing

lexIdent :: Input -> Maybe ((String, Span), Input)
lexIdent (inputAccept (\c -> isAlpha c || (c == '_')) -> Just ((c, startSpan), input)) =
    case inputSpan (\c -> isAlphaNum c || (c == '_')) input of
      Nothing -> Just (([c], startSpan), input)
      Just ((rest, endSpan), input') -> Just ((c : rest, startSpan `union` endSpan), input')
lexIdent _ = Nothing

lexStringLit :: Input -> Maybe ((String, Span), Input)
lexStringLit input = do
  (start, input') <- inputConsume '"' input
  ((string, _), input'') <- inputSpan (/= '"') input'
  (end, input''') <- inputConsume '"' input''

  Just ((string, start `union` end), input''')

lexIndents :: Input -> Maybe ((Int, Span), Input)
lexIndents (inputConsume '\t' -> Just (start, input)) =
  case lexIndents input of
    Just ((count, end), input') -> Just ((count + 1, start `union` end), input')
    Nothing -> Just ((1, start), input)

lexIndents _ = Nothing
