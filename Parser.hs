{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}

module Parser(Func(..), FuncArg(..), Stmt(..), Expr(..), parse) where

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
  | None
  deriving (Show)

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
    case parseInnerExpr rest (Assign name) of
      Just (stmt, rest') -> stmt : parseBody rest'
      Nothing -> PoisonStmt "Assignment to invalid expression" start : parseBody (dropWhile (not . isNewLine) rest)

parseBody ( Lexer.TokenInfo Lexer.Return _
          : Lexer.TokenInfo Lexer.NewLine _
          : rest ) =
    Return None : parseBody rest

parseBody ( Lexer.TokenInfo Lexer.Return start
          : rest ) =
    case parseInnerExpr rest Return of
      Just (stmt, rest') -> stmt : parseBody rest'
      Nothing -> PoisonStmt "Return invalid expression" start : parseBody (dropWhile (not . isNewLine) rest)

parseBody (token : _) = error ("Unhandled token " ++ show token)

-- |Parse an expression that forms a greater whole.
parseInnerExpr :: [Lexer.TokenInfo] -> (Expr -> a) -> Maybe (a, [Lexer.TokenInfo])
parseInnerExpr tokens build = do
  (expr, tokens') <- parseExpr tokens
  Just (build expr, tokens')

parseExpr :: [Lexer.TokenInfo] -> Maybe (Expr, [Lexer.TokenInfo])
parseExpr (Lexer.TokenInfo (Lexer.IntLit value) _ : rest) = Just (IntLit value, rest)
parseExpr (Lexer.TokenInfo (Lexer.Ident value) _ : rest) = Just (Ref value, rest)
parseExpr _ = Nothing

isNewLine :: Lexer.TokenInfo -> Bool
isNewLine (Lexer.TokenInfo Lexer.NewLine _) = True
isNewLine _ = False
