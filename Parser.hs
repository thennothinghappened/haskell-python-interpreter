{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Parser(Func(..), FuncArg(..), Stmt(..), Expr(..), BinOp(..), parse) where

import Lexer (TokenInfo(..), Span)
import qualified Lexer as Token (Token(..))
import Data.Maybe (fromMaybe)

-- |A callable function in the program.
data Func = Func {
  args :: [FuncArg],
  body :: [Stmt]
} deriving (Show)

-- |An argument to a callable function with a locally bound name and default value when none is
--  provided.
data FuncArg = FuncArg {
  name :: String,
  defaultValue :: Expr
} deriving (Show)

-- |An executable statement in a program.
data Stmt
  = Assign { name :: String, expr :: Expr }
  | DefineFunc { name :: String, func :: Func }
  | Return Expr
  | PoisonStmt { message :: String, span :: Span }
  deriving (Show)

-- |An expression that evaluates to a value.
data Expr
  = IntLit Int
  | StringLit String
  | Ref String
  | BinOp BinOp Expr Expr
  | Call Expr [Expr]
  | None
  deriving (Show)

-- |An operation taking two operands.
data BinOp
  = Add
  | Sub

instance Show BinOp where
  show :: BinOp -> String
  show Add = "+"
  show Sub = "-"

-- |Parse a full source file into a top-level callable function.
parse :: [TokenInfo] -> [Stmt]
parse tokens =
  let (statements, _) = parseBody 0 tokens
   in statements

-- |Parse a function body into a list of statements.
parseBody :: Int -> [TokenInfo] -> ([Stmt], [TokenInfo])
parseBody _ [] = ([], [])

-- If the next line is indented less, it doesn't belong to us.
parseBody indents tokens@(TokenInfo (Token.NewLine  nextIndents) pos : rest)
  | nextIndents < indents = ([], tokens)
  | nextIndents == indents = parseBody indents rest
  | nextIndents > indents = PoisonStmt "Too much indentation" pos `thenRest` parseBody indents rest

parseBody indents
          ( TokenInfo (Token.Ident name) start
          : TokenInfo Token.SingleEquals _
          : rest ) =
    case parseInnerExpr rest of
      Just (expr, rest') -> Assign name expr `thenRest` parseBody indents rest'

      Nothing ->
        PoisonStmt "Assignment to invalid expression" start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

parseBody indents
          ( TokenInfo Token.Def start
          : TokenInfo (Token.Ident name) _
          : rest ) =
    case parseFuncArgs rest of
      Just (args, TokenInfo Token.Colon _ : rest') ->
        let (body, rest'') = parseBody (indents + 1) rest'
         in DefineFunc { name, func = Func args body } `thenRest` parseBody indents rest''

      Nothing ->
        PoisonStmt ("Malformed argument list for function " ++ show name) start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

      _ ->
        PoisonStmt ("Missing colon on function definition " ++ show name) start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

parseBody indents
          ( TokenInfo Token.Return _
          : TokenInfo (Token.NewLine _) _
          : rest ) =
    Return None `thenRest` parseBody indents rest

parseBody indents
          ( TokenInfo Token.Return start
          : rest ) =
    case parseInnerExpr rest of
      Just (expr, rest') -> Return expr `thenRest` parseBody indents rest'

      Nothing ->
        PoisonStmt "Return invalid expression" start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

parseBody _ (token : _) = error ("Unhandled token " ++ show token)

thenRest :: Stmt -> ([Stmt], [TokenInfo]) -> ([Stmt], [TokenInfo])
stmt `thenRest` (rest, tokens) = (stmt : rest, tokens)

-- |Parse a top-level expression.
parseInnerExpr :: [TokenInfo] -> Maybe (Expr, [TokenInfo])
parseInnerExpr tokens = do
  (left, tokens') <- parseTerminalExpr tokens
  parsePostfixExpr left tokens'

parsePostfixExpr :: Expr -> [TokenInfo] -> Maybe (Expr, [TokenInfo])
parsePostfixExpr left tokens =
  case tokens of
    (TokenInfo Token.Plus _ : tokens') -> do
      (right, tokens'') <- parseInnerExpr tokens'
      Just (BinOp Add left right, tokens'')

    (TokenInfo Token.Minus _ : tokens') -> do
      (right, tokens'') <- parseInnerExpr tokens'
      Just (BinOp Sub left right, tokens'')

    (TokenInfo Token.OpenParen _ : tokens') -> do
      (args, tokens'') <- parseFuncCall tokens'
      let call = Call left args

      Just $ fromMaybe (call, tokens'') (parsePostfixExpr call tokens'')

    _ -> Just (left, tokens)

parseTerminalExpr :: [TokenInfo] -> Maybe (Expr, [TokenInfo])
parseTerminalExpr (TokenInfo (Token.IntLit value) _ : rest) = Just (IntLit value, rest)
parseTerminalExpr (TokenInfo (Token.Ident value) _ : rest) = Just (Ref value, rest)
parseTerminalExpr (TokenInfo (Token.StringLit value) _ : rest) = Just (StringLit value, rest)
parseTerminalExpr _ = Nothing

parseFuncArgs :: [TokenInfo] -> Maybe ([FuncArg], [TokenInfo])
parseFuncArgs (TokenInfo Token.OpenParen _ : tokens) = do
    parseArgList tokens
  where
    parseArgList :: [TokenInfo] -> Maybe ([FuncArg], [TokenInfo])
    parseArgList (TokenInfo Token.CloseParen _ : tokens) = Just ([], tokens)

    parseArgList (TokenInfo (Token.Ident name) _ : TokenInfo Token.SingleEquals _ : tokens) = do
      (defaultValue, tokens') <- parseInnerExpr tokens

      case tokens' of
        (TokenInfo Token.Comma _ : rest) -> do
          (restArgs, tokens'') <- parseArgList rest
          Just (FuncArg name defaultValue : restArgs, tokens'')

        (TokenInfo Token.CloseParen _ : rest) -> Just ([FuncArg name defaultValue], rest)

        _ -> Nothing

    parseArgList (TokenInfo (Token.Ident name) _ : TokenInfo Token.Comma _ : tokens) = do
      (restArgs, tokens') <- parseArgList tokens
      Just (FuncArg name None : restArgs, tokens')

    parseArgList (TokenInfo (Token.Ident name) _ : TokenInfo Token.CloseParen _ : tokens) =
      Just ([FuncArg name None], tokens)

    parseArgList _ = Nothing

parseFuncArgs _ = Nothing

parseFuncCall :: [TokenInfo] -> Maybe ([Expr], [TokenInfo])
parseFuncCall (TokenInfo Token.CloseParen _ : rest) = Just ([], rest)

-- eatIndents :: [TokenInfo] -> [TokenInfo]
-- eatIndents (TokenInfo (Token.Indents _) _ : rest) = eatIndents rest
-- eatIndents tokens = tokens

isNewLine :: TokenInfo -> Bool
isNewLine (TokenInfo (Token.NewLine _) _) = True
isNewLine _ = False
