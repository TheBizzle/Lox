module Lox.Parser.Internal.AST(
    AST(AST, statements)
  , Expr(arguments, Assign, Binary, Call, callee, expression, Get, Grouping, keyword, left, literal, LiteralExpr, literalToken, Logical, object, operator, paren, right, Set, Super, This, Unary, value, var, VarRef)
  , exprToToken
  , Function(fnBody, fnDecl, Function, params)
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Statement(Block, body, Class, contents, DeclareVar, expr, ExpressionStatement, FunctionStatement, IfElse, loc, newVar, predicate, PrintStatement, ReturnStatement, token, WhileStatement)
  , Variable(Variable, varName, varToken)
  ) where

import Lox.Scanner.Token(SourceLoc, Token)

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
  | ReturnStatement { token :: Token, exprM :: Maybe Expr }
  | WhileStatement { predicate :: Expr, body :: Statement }
  deriving (Eq, Show)

data Literal
  = BooleanLit Bool
  | DoubleLit  Double
  | StringLit  Text
  | NilLit
  deriving (Eq, Show)

data Variable
  = Variable { varName :: Text, varToken :: Token }
  deriving (Eq, Show)

data Expr
  = Assign      { var :: Variable, value :: Expr }
  | Binary      { left :: Expr, operator :: Token, right :: Expr }
  | Call        { callee :: Expr, paren :: Token, arguments :: [Expr] }
  | Get         { object :: Expr, var :: Variable }
  | Grouping    { expression :: Expr }
  | LiteralExpr { literal :: Literal, literalToken :: Token }
  | Logical     { left :: Expr, operator :: Token, right :: Expr }
  | Set         { object :: Expr, var :: Variable, value :: Expr }
  | Super       { keyword :: Token, var :: Variable }
  | This        { keyword :: Token }
  | Unary       { operator :: Token, right :: Expr }
  | VarRef      { var :: Variable }
  deriving (Eq, Show)

exprToToken :: Expr -> Token
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
