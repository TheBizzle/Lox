{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Parser.Internal.Program where

import Lox.Scanner.Token

data Program
  = Program { program :: Expr }
  deriving Show

data Literal
  = BooleanLit Bool
  | DoubleLit  Double
  | StringLit  Text
  | NilLit
  deriving Show

data Expr
  = Assign      { name :: Token, value :: Expr }
  | Binary      { left :: Expr, operator :: Token, right :: Expr }
  | Call        { callee :: Expr, paren :: Token, arguments :: [Expr] }
  | Get         { object :: Expr, name :: Token }
  | Grouping    { expression :: Expr }
  | LiteralExpr { literal :: Literal }
  | Logical     { left :: Expr, operator :: Token, right :: Expr }
  | Set         { object :: Expr, name :: Token, value :: Expr }
  | Super       { keyword :: Token, method :: Token }
  | This        { keyword :: Token }
  | Unary       { operator :: Token, right :: Expr }
  | Variable    { name :: Token }
  deriving Show
