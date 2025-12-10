{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Parser.Internal.Program(
    Expr(..),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(Program, statements),
    Statement(DeclareVar, expr, ExpressionStatement, newVarName, PrintStatement, term)
  ) where

import Lox.Scanner.Token(TokenPlus)

data Program
  = Program { statements :: [Statement] }
  deriving Show

data Statement
  = DeclareVar { newVarName :: TokenPlus, initial :: Expr }
  | ExpressionStatement { expr :: Expr }
  | PrintStatement { term :: TokenPlus, expr :: Expr }
  deriving Show

data Literal
  = BooleanLit Bool
  | DoubleLit  Double
  | StringLit  Text
  | NilLit
  deriving Show

data Expr
  = Assign      { name :: TokenPlus, value :: Expr }
  | Binary      { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Call        { callee :: Expr, paren :: TokenPlus, arguments :: [Expr] }
  | Get         { object :: Expr, name :: TokenPlus }
  | Grouping    { expression :: Expr }
  | LiteralExpr { literal :: Literal }
  | Logical     { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Set         { object :: Expr, name :: TokenPlus, value :: Expr }
  | Super       { keyword :: TokenPlus, method :: TokenPlus }
  | This        { keyword :: TokenPlus }
  | Unary       { operator :: TokenPlus, right :: Expr }
  | Variable    { name :: TokenPlus }
  deriving Show
