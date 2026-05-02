module Lox.Parser.Internal.ExpressionParser(expression, primary, unary) where

import Lox.Scanner.Token(
    Token(Token)
  , TokenType(And, Bang, BangEqual, Comma, Dot, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Or, Plus, RightParen, This, TokenFalse, TokenTrue, Slash, Super, Star, String)
  )

import Lox.Parser.Internal.AST(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Unary, VarRef)
  , exprToToken
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Variable
  )

import Lox.Parser.Internal.Parser(
    (=#>), anyOf, atMost, bail, convert, cryAbout, errorWith, notFollowedBy, one
  , Parser(Parser)
  , parserFrom
  , throwaway, variable, win
  )

import Lox.Parser.Internal.ParserError(
    ParserError(ParserError)
  , ParserErrorType(ExpectedDotAfterSuper, ExpectedParenAfterArgs, ExpectedPropertyName, ExpectedSuperMethodName, InvalidAssign, InvalidExpression, Missing, TooMuchArguing)
  )

import qualified Lox.Parser.Internal.AST as AST


expression :: Parser Expr
expression = assignment

assignment :: Parser Expr
assignment = setOperation <|> assignOperation <|> logicalOr
  where
    assignOperation =
      do
        target <- logicalOr
        token  <- one Equal
        value  <- assignment
        case target of
          VarRef name  -> pure $ Assign name value
          Get obj name -> pure $ Set obj name value
          _            -> Parser $ const $ errorWith $ ParserError InvalidAssign token

    setOperation =
      do
        obj <- fnCall
        throwaway Dot
        var <- variable
        throwaway Equal
        ass <- assignment <|> cryAbout InvalidAssign
        return $ Set obj var ass

logicalOr :: Parser Expr
logicalOr = orOperation <|> logicalAnd
  where
    orOperation =
      do
        operand1 <- logicalAnd
        operator <- one Or
        operand2 <- logicalOr
        pure $ Logical operand1 operator operand2

logicalAnd :: Parser Expr
logicalAnd = andOperation <|> equality
  where
    andOperation =
      do
        operand1 <- equality
        operator <- one And
        operand2 <- logicalAnd
        pure $ Logical operand1 operator operand2

equality :: Parser Expr
equality = eqOperation <|> comparison
  where
    eqOperation =
      do
        operand1 <- comparison
        operator <- anyOf [BangEqual, EqualEqual]
        operand2 <- equality
        pure $ Binary operand1 operator operand2

comparison :: Parser Expr
comparison = compOperation <|> term
  where
    compOperation =
      do
        operand1 <- term
        operator <- anyOf [Greater, GreaterEqual, Less, LessEqual]
        operand2 <- comparison
        pure $ Binary operand1 operator operand2

term :: Parser Expr
term = termOperation <|> factor
  where
    termOperation =
      do
        operand1 <- factor
        operator <- anyOf [Minus, Plus]
        operand2 <- term
        pure $ Binary operand1 operator operand2

factor :: Parser Expr
factor = factorOperation <|> unary
  where
    factorOperation =
      do
        operand1 <- unary
        operator <- anyOf [Slash, Star]
        operand2 <- factor
        pure $ Binary operand1 operator operand2

unary :: Parser Expr
unary = unaryOperation <|> fnCall
  where
    unaryOperation =
      do
        operator <- anyOf [Bang, Minus]
        operand  <- unary
        pure $ Unary operator operand

data CallType
  = Simple Token [Expr]
  | Method Variable

fnCall :: Parser Expr
fnCall = call <|> primary
  where
    call       = makeCalls <$> primary <*> (some invocation)
    invocation = simpleCall <|> getMethod

    simpleCall =
      do
        token     <- one LeftParen
        arguments <- args    <|> pure []
        throwaway RightParen <|> cryAbout ExpectedParenAfterArgs
        pure $ Simple token arguments

    getMethod =
      do
        throwaway Dot
        name <- variable <|> cryAbout ExpectedPropertyName
        notFollowedBy $ one Equal
        pure $ Method name

    args =
      do
        arg1   <- expression
        argies <- atMost TooMuchArguing 254 exprToToken $ (throwaway Comma) *> expression
        pure $ arg1:argies

    makeCalls _ []   = error "Not possible!  `some` produces a list of at least length 1!"
    makeCalls p invs = foldl' mkCall p invs

    mkCall acc (Simple lpt args) = Call acc lpt args
    mkCall acc (Method var     ) = Get  acc var

primary :: Parser Expr
primary = number <|> string <|> true <|> false <|> nil <|> this <|> fullVariable <|> grouping <|> super
  where
    true  = convert $ TokenTrue  =#> (LiteralExpr $ BooleanLit True)
    false = convert $ TokenFalse =#> (LiteralExpr $ BooleanLit False)
    nil   = convert $        Nil =#>  LiteralExpr   NilLit
    this  = convert $       This =#>  AST.This

    grouping =
      do
        throwaway LeftParen
        expr <- expression   <|> cryAbout  InvalidExpression
        throwaway RightParen <|> cryAbout (Missing RightParen)
        pure $ Grouping expr

    super =
      do
        token <- one Super
        _     <- throwaway Dot <|> cryAbout ExpectedDotAfterSuper
        var   <- variable      <|> cryAbout ExpectedSuperMethodName
        pure $ AST.Super token var

    number = parserFrom helper
      where
        helper t@(Token (Number x) _) = win $ LiteralExpr (DoubleLit x) t
        helper _                      = bail

    string = parserFrom helper
      where
        helper t@(Token (String s) _) = win $ LiteralExpr (StringLit s) t
        helper _                      = bail

fullVariable :: Parser Expr
fullVariable = VarRef <$> variable
