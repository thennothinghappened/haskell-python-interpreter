{-# LANGUAGE ViewPatterns, OverloadedRecordDot, DuplicateRecordFields #-}

module Lexer(Token(..), TokenInfo(..), Location, Span, lexString) where

import Prelude hiding (lex)
import Data.Char (isAlphaNum, isAlpha, isNumber)
import Text.Read (readMaybe)
import Unicode.Char (isWhiteSpace)

-- |Convert the passed string into a sequence of lexical tokens that can be later parsed.
lexString :: String -> [TokenInfo]
lexString inputString = lex (Input inputString startOfFile)

data Token
  = Ident String
  | IntLit Int
  | SingleEquals
  | Return
  | NewLine
  | Indents Int
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

startOfFile :: Location
startOfFile = Location { row = 1, column = 1 }

data Span = Span {
  start :: Location,
  end :: Location
}

emptySpan :: Location -> Span
emptySpan location = Span location location

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
inputNext (Input (c : rest) loc) =
  Input
    rest
    (updateLocation loc c)
inputNext emptyInput = emptyInput

-- |Drop N characters from the input.
inputDrop :: Word -> Input -> Input
inputDrop 0 input = input
inputDrop n input = inputDrop (n - 1) (inputNext input)

inputSpan :: (Char -> Bool) -> Input -> ((Span, String), Input)
inputSpan f input@(Input (c : _) start)
  | f c =
      let ((Span { end }, match), unmatched) = inputSpan f (inputNext input)
        in ((Span { start, end }, c : match), unmatched)
inputSpan _ input = ((emptySpan input.start, []), input)

-- |Drop characters from the start of the input file which match the given predicate.
inputDropWhile :: (Char -> Bool) -> Input -> Input
inputDropWhile f input =
  case inputSpan f input of
    ((_, []), _) -> input
    ((_, _), rest) -> rest

-- |Remove the given prefix from the input if present, returning the rest of the input if
--  successful, and the span from start to end of the prefix.
inputStripPrefix :: String -> Input -> Maybe (Span, Input)
inputStripPrefix [] input = Just (emptySpan input.start, input)
inputStripPrefix (c : restPrefix) (Input (textC : restText) start)
  | c == textC = do
      (Span _ end, input') <- inputStripPrefix restPrefix (Input restText (updateLocation start c))
      Just (Span start end, input')
inputStripPrefix _ _ = Nothing

-- |Increment the row or column of the given location as appropriate to the character there.
updateLocation :: Location -> Char -> Location
updateLocation location '\r' = location
updateLocation location '\n' = nextRow location
updateLocation location _ = nextColumn location

lex :: Input -> [TokenInfo]
lex (Input [] _) = []
lex (inputStripPrefix "return" -> Just (span, input)) = TokenInfo Return span : lex input
lex (inputStripPrefix "=" -> Just (span, input)) = TokenInfo SingleEquals span : lex input
lex input
  | c == '\n' =
    TokenInfo
      NewLine
      (Span input.start (nextRow input.start)) : lex (inputNext input)
  | c == '\t' =
    let ((span, _), rest) = inputSpan (== '\t') input
        in TokenInfo (Indents (span.end.column - span.start.column)) span : lex rest
  | isValidIdentStart c =
      let ((span, ident), rest) = inputSpan isValidIdent input
        in TokenInfo (Ident ident) span : lex rest
  | isNumber c =
      let ((span, valueString), rest) = inputSpan isNumber input
        in case readMaybe valueString of
          Just value -> TokenInfo (IntLit value) span : lex rest
          Nothing -> undefined
  | isWhiteSpace c =
      lex (inputDropWhile isWhiteSpace input)
  | otherwise = []
  where
    c = head input.text

isValidIdent :: Char -> Bool
isValidIdent c = isAlphaNum c || (c == '_')

isValidIdentStart :: Char -> Bool
isValidIdentStart c = isAlpha c || (c == '_')
