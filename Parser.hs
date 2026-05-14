{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Parser(Func(..), FuncArg(..), Stmt(..), Expr(..), BinOp(..), parse) where

import qualified Lexer(Token(..), TokenInfo(..), Span)

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
  | Return Expr
  | PoisonStmt { message :: String, span :: Lexer.Span }
  deriving (Show)

-- |An expression that evaluates to a value.
data Expr
  = IntLit Int
  | Ref String
  | BinOp BinOp Expr Expr
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
parse :: [Lexer.TokenInfo] -> [Stmt]
parse = parseBody

-- |Parse a function body into a list of statements.
parseBody :: [Lexer.TokenInfo] -> [Stmt]
parseBody [] = []
parseBody (Lexer.TokenInfo Lexer.NewLine _ : rest) = parseBody rest

parseBody ( Lexer.TokenInfo (Lexer.Ident name) start
          : Lexer.TokenInfo Lexer.SingleEquals _
          : rest ) =
    case parseInnerExpr rest of
      Just (expr, rest') -> Assign name expr : parseBody rest'
      Nothing -> PoisonStmt "Assignment to invalid expression" start : parseBody (dropWhile (not . isNewLine) rest)

parseBody ( Lexer.TokenInfo Lexer.Return _
          : Lexer.TokenInfo Lexer.NewLine _
          : rest ) =
    Return None : parseBody rest

parseBody ( Lexer.TokenInfo Lexer.Return start
          : rest ) =
    case parseInnerExpr rest of
      Just (expr, rest') -> Return expr : parseBody rest'
      Nothing -> PoisonStmt "Return invalid expression" start : parseBody (dropWhile (not . isNewLine) rest)

parseBody (token : _) = error ("Unhandled token " ++ show token)

-- |Parse a top-level expression.
parseInnerExpr :: [Lexer.TokenInfo] -> Maybe (Expr, [Lexer.TokenInfo])
parseInnerExpr tokens = do
  (left, tokens') <- parseTerminalExpr tokens

  case tokens' of
    (Lexer.TokenInfo Lexer.Plus _ : tokens'') -> do
      (right, tokens''') <- parseInnerExpr tokens''
      Just (BinOp Add left right, tokens''')

    (Lexer.TokenInfo Lexer.Minus _ : tokens'') -> do
      (right, tokens''') <- parseInnerExpr tokens''
      Just (BinOp Sub left right, tokens''')
      
    _ -> Just (left, tokens')

parseTerminalExpr :: [Lexer.TokenInfo] -> Maybe (Expr, [Lexer.TokenInfo])
parseTerminalExpr (Lexer.TokenInfo (Lexer.IntLit value) _ : rest) = Just (IntLit value, rest)
parseTerminalExpr (Lexer.TokenInfo (Lexer.Ident value) _ : rest) = Just (Ref value, rest)
parseTerminalExpr _ = Nothing

isNewLine :: Lexer.TokenInfo -> Bool
isNewLine (Lexer.TokenInfo Lexer.NewLine _) = True
isNewLine _ = False
