module Lox.Parser.Internal.Program(
    Expr(..),
    exprToToken,
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(Program, statements),
    Statement(Block, contents, DeclareVar, expr, ExpressionStatement, newVarName, PrintStatement, term)
  ) where

import Lox.Scanner.Token(TokenPlus)

data Program
  = Program { statements :: [Statement] }
  deriving Show

data Statement
  = Block { contents :: [Statement] }
  | DeclareVar { newVarName :: Text, token :: TokenPlus, initial :: Expr }
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
  = Assign      { name :: Text, nameToken :: TokenPlus, value :: Expr }
  | Binary      { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Call        { callee :: Expr, paren :: TokenPlus, arguments :: [Expr] }
  | Get         { object :: Expr, name :: Text, nameToken :: TokenPlus }
  | Grouping    { expression :: Expr }
  | LiteralExpr { literal :: Literal, literalToken :: TokenPlus }
  | Logical     { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Set         { object :: Expr, name :: Text, nameToken :: TokenPlus, value :: Expr }
  | Super       { keyword :: TokenPlus, method :: TokenPlus }
  | This        { keyword :: TokenPlus }
  | Unary       { operator :: TokenPlus, right :: Expr }
  | Variable    { name :: Text, nameToken :: TokenPlus }
  deriving Show

exprToToken :: Expr -> TokenPlus
exprToToken (Assign      _      token _  ) = token
exprToToken (Binary      left   _     _  ) = exprToToken left
exprToToken (Call        callee _     _  ) = exprToToken callee
exprToToken (Get         obj    _     _  ) = exprToToken obj
exprToToken (Grouping    expr            ) = exprToToken expr
exprToToken (LiteralExpr _      token    ) = token
exprToToken (Logical     left   _     _  ) = exprToToken left
exprToToken (Set         obj    _     _ _) = exprToToken obj
exprToToken (Super       kword  _        ) = kword
exprToToken (This        kword           ) = kword
exprToToken (Unary       op     _        ) = op
exprToToken (Variable    _      token    ) = token
