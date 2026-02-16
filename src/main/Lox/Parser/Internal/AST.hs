module Lox.Parser.Internal.AST(
    AST(AST, statements)
  , Expr(..)
  , exprToToken
  , Function(fnBody, fnDecl, Function, params)
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Statement(Block, body, Class, contents, DeclareVar, expr, ExpressionStatement, FunctionStatement, IfElse, loc, newVar, predicate, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(Variable, varName, varToken)
  ) where

import Lox.Scanner.Token(SourceLoc, TokenPlus)

data AST
  = AST { statements :: [Statement] }
  deriving Show

data Function
  = Function { fnDecl :: Variable, params :: [Variable], fnBody :: [Statement] }
  deriving (Eq, Show)

data Statement
  = Block { contents :: [Statement] }
  | Class { classVar :: Variable, superNameVarM :: Maybe Variable, methods :: [Function] }
  | DeclareVar { newVar :: Variable, initial :: Expr }
  | ExpressionStatement { expr :: Expr }
  | FunctionStatement { func :: Function }
  | IfElse { antecedent :: Expr, consequent :: Statement, alternative :: Maybe Statement }
  | PrintStatement { loc :: SourceLoc, expr :: Expr }
  | ReturnStatement { loc :: SourceLoc, exprM :: Maybe Expr }
  | WhileStatement { predicate :: Expr, body :: Statement }
  deriving (Eq, Show)

data Literal
  = BooleanLit Bool
  | DoubleLit  Double
  | StringLit  Text
  | NilLit
  deriving (Eq, Show)

data Variable
  = Variable { varName :: Text, varToken :: TokenPlus }
  deriving (Eq, Show)

data Expr
  = Assign      { var :: Variable, value :: Expr }
  | Binary      { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Call        { callee :: Expr, paren :: TokenPlus, arguments :: [Expr] }
  | Get         { object :: Expr, var :: Variable }
  | Grouping    { expression :: Expr }
  | LiteralExpr { literal :: Literal, literalToken :: TokenPlus }
  | Logical     { left :: Expr, operator :: TokenPlus, right :: Expr }
  | Set         { object :: Expr, var :: Variable, value :: Expr }
  | Super       { keyword :: TokenPlus, var :: Variable }
  | This        { keyword :: TokenPlus }
  | Unary       { operator :: TokenPlus, right :: Expr }
  | VarRef      { var :: Variable }
  deriving (Eq, Show)

exprToToken :: Expr -> TokenPlus
exprToToken a@(Assign      _      _      ) = a.var.varToken
exprToToken   (Binary      left   _     _) = exprToToken left
exprToToken   (Call        callee _     _) = exprToToken callee
exprToToken   (Get         obj    _      ) = exprToToken obj
exprToToken   (Grouping    expr          ) = exprToToken expr
exprToToken   (LiteralExpr _      token  ) = token
exprToToken   (Logical     left   _     _) = exprToToken left
exprToToken   (Set         obj    _     _) = exprToToken obj
exprToToken   (Super       kword  _      ) = kword
exprToToken   (This        kword         ) = kword
exprToToken   (Unary       op     _      ) = op
exprToToken v@(VarRef      _             ) = v.var.varToken
