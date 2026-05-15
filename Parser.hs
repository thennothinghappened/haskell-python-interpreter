{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Parser(Func(..), FuncArg(..), Stmt(..), Expr(..), BinOp(..), parse) where

import Lexer (TokenInfo(..), Span)
import qualified Lexer as Token (Token(..))
import Data.Maybe (fromMaybe)
import Control.Monad.Combinators ((<|>))

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
    case parseExpr rest of
      Just (expr, rest') -> Assign name expr `thenRest` parseBody indents rest'

      Nothing ->
        PoisonStmt "Assignment to invalid expression" start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

parseBody indents
          ( TokenInfo Token.Def start
          : TokenInfo (Token.Ident name) _
          : rest ) =
    case inParenthesis parseFuncArgs rest of
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
    case parseExpr rest of
      Just (expr, rest') -> Return expr `thenRest` parseBody indents rest'

      Nothing ->
        PoisonStmt "Return invalid expression" start
        `thenRest` parseBody indents (dropWhile (not . isNewLine) rest)

parseBody _ (token : _) = error ("Unhandled token " ++ show token)

thenRest :: Stmt -> ([Stmt], [TokenInfo]) -> ([Stmt], [TokenInfo])
stmt `thenRest` (rest, tokens) = (stmt : rest, tokens)

-- |Parse a top-level expression.
parseExpr :: [TokenInfo] -> Maybe (Expr, [TokenInfo])
parseExpr tokens = do
  (left, tokens') <- parseTerminalExpr tokens
  parsePostfixExpr left tokens'

parsePostfixExpr :: Expr -> [TokenInfo] -> Maybe (Expr, [TokenInfo])
parsePostfixExpr left tokens =
  case tokens of
    (TokenInfo Token.Plus _ : tokens') -> do
      (right, tokens'') <- parseExpr tokens'
      Just (BinOp Add left right, tokens'')

    (TokenInfo Token.Minus _ : tokens') -> do
      (right, tokens'') <- parseExpr tokens'
      Just (BinOp Sub left right, tokens'')

    (inParenthesis (Just . parseCallArgs) -> Just (args, tokens')) ->
      let call = Call left args
       in parsePostfixExpr call tokens' <|> Just (call, tokens')

    _ -> Just (left, tokens)

parseTerminalExpr :: [TokenInfo] -> Maybe (Expr, [TokenInfo])
parseTerminalExpr (TokenInfo (Token.IntLit value) _ : rest) = Just (IntLit value, rest)
parseTerminalExpr (TokenInfo (Token.Ident value) _ : rest) = Just (Ref value, rest)
parseTerminalExpr (TokenInfo (Token.StringLit value) _ : rest) = Just (StringLit value, rest)
parseTerminalExpr _ = Nothing

parseFuncArgs :: [TokenInfo] -> Maybe ([FuncArg], [TokenInfo])
parseFuncArgs (TokenInfo (Token.Ident name) _ : TokenInfo Token.SingleEquals _ : tokens) = do
  (defaultValue, tokens') <- parseExpr tokens
  let arg = FuncArg name defaultValue

  case tokens' of
    (TokenInfo Token.Comma _ : rest) -> do
      (restArgs, tokens'') <- parseFuncArgs rest
      Just (arg : restArgs, tokens'')

    rest -> Just ([arg], rest)

parseFuncArgs (TokenInfo (Token.Ident name) _ : TokenInfo Token.Comma _ : tokens) = do
  (restArgs, tokens') <- parseFuncArgs tokens
  Just (FuncArg name None : restArgs, tokens')

parseFuncArgs (TokenInfo (Token.Ident name) _ : tokens) = Just ([FuncArg name None], tokens)
parseFuncArgs tokens = Just ([], tokens)

-- |Parse a comma-separated list of arguments in calling a function.
parseCallArgs :: [TokenInfo] -> ([Expr], [TokenInfo])
parseCallArgs (parseExpr -> Just (arg, TokenInfo Token.Comma _ : rest)) =
  let (restArgs, rest') = parseCallArgs rest
   in (arg : restArgs, rest')

parseCallArgs (parseExpr -> Just (arg, rest)) = ([arg], rest)
parseCallArgs rest = ([], rest)

-- |Parse something that's surrounded by open and closing parenthesis.
inParenthesis :: ([TokenInfo] -> Maybe (a, [TokenInfo])) -> [TokenInfo] -> Maybe (a, [TokenInfo])
inParenthesis f (TokenInfo Token.OpenParen _ : rest) = do
  (inner, TokenInfo Token.CloseParen _ : rest') <- f rest
  Just (inner, rest')
inParenthesis _ _ = Nothing

isNewLine :: TokenInfo -> Bool
isNewLine (TokenInfo (Token.NewLine _) _) = True
isNewLine _ = False
