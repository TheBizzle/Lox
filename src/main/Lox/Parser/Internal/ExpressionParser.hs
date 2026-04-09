module Lox.Parser.Internal.ExpressionParser(expression, primary, unary) where

import Lox.Scanner.Token(
    Token(And, Bang, BangEqual, Comma, Dot, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Or, Plus, RightParen, This, TokenFalse, TokenTrue, Slash, Super, Star, String)
  , TokenPlus(TokenPlus)
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
        target  <- logicalOr
        equalTP <- one Equal
        value   <- assignment
        case target of
          VarRef name  -> pure $ Assign name value
          Get obj name -> pure $ Set obj name value
          _            -> Parser $ const $ errorWith $ ParserError InvalidAssign equalTP

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
        operator1 <- logicalAnd
        operand   <- one Or
        operator2 <- logicalOr
        pure $ Logical operator1 operand operator2

logicalAnd :: Parser Expr
logicalAnd = andOperation <|> equality
  where
    andOperation =
      do
        operator1 <- equality
        operand   <- one And
        operator2 <- logicalAnd
        pure $ Logical operator1 operand operator2

equality :: Parser Expr
equality = eqOperation <|> comparison
  where
    eqOperation =
      do
        operator1 <- comparison
        operand   <- anyOf [BangEqual, EqualEqual]
        operator2 <- equality
        pure $ Binary operator1 operand operator2

comparison :: Parser Expr
comparison = compOperation <|> term
  where
    compOperation =
      do
        operator1 <- term
        operand   <- anyOf [Greater, GreaterEqual, Less, LessEqual]
        operator2 <- comparison
        pure $ Binary operator1 operand operator2

term :: Parser Expr
term = termOperation <|> factor
  where
    termOperation =
      do
        operator1 <- factor
        operand   <- anyOf [Minus, Plus]
        operator2 <- term
        pure $ Binary operator1 operand operator2

factor :: Parser Expr
factor = factorOperation <|> unary
  where
    factorOperation =
      do
        operator1 <- unary
        operand   <- anyOf [Slash, Star]
        operator2 <- factor
        pure $ Binary operator1 operand operator2

unary :: Parser Expr
unary = unaryOperation <|> fnCall
  where
    unaryOperation =
      do
        operator <- anyOf [Bang, Minus]
        operand  <- unary
        pure $ Unary operator operand

data CallType
  = Simple TokenPlus [Expr]
  | Method Variable

fnCall :: Parser Expr
fnCall = call <|> primary
  where
    call       = makeCalls <$> primary <*> (some invocation)
    invocation = simpleCall <|> getMethod

    simpleCall =
      do
        parenTP   <- one LeftParen
        arguments <- args    <|> pure []
        throwaway RightParen <|> cryAbout ExpectedParenAfterArgs
        pure $ Simple parenTP arguments

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

    makeCalls p (ctype:is) = foldl (\acc ct -> mkCall acc ct) (mkCall p ctype) is
    makeCalls _         [] = error "Not possible!  `some` produces a list of at least length 1!"

    mkCall acc (Simple lpTP args) = Call acc lpTP args
    mkCall acc (Method var      ) = Get  acc var

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
        superTP <- one Super
        _       <- throwaway Dot <|> cryAbout ExpectedDotAfterSuper
        var     <- variable      <|> cryAbout ExpectedSuperMethodName
        pure $ AST.Super superTP var

    number = parserFrom helper
      where
        helper tp@(TokenPlus (Number x) _) = win $ LiteralExpr (DoubleLit x) tp
        helper _                           = bail

    string = parserFrom helper
      where
        helper tp@(TokenPlus (String s) _) = win $ LiteralExpr (StringLit s) tp
        helper _                           = bail

fullVariable :: Parser Expr
fullVariable = VarRef <$> variable
