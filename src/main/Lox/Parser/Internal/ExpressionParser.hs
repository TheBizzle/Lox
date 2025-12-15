module Lox.Parser.Internal.ExpressionParser(expression, unary) where

import Lox.Scanner.Token(
    Token(And, Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Or, Plus, RightParen, TokenFalse, TokenTrue, Slash, Star, String),
    TokenPlus(TokenPlus)
  )

import Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, one, oneOf, Parser, parserFrom, throwaway, variable, win
  )

import Lox.Parser.Internal.Program(
    Expr(Assign, Binary, Grouping, LiteralExpr, Logical, Unary, Variable),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  )


expression :: Parser Expr
expression = expr

expr :: Parser Expr
expr = assignment

assignment :: Parser Expr
assignment = assignOperation <|> logicalOr
  where
    assignOperation = (\(name, tp) v -> Assign name tp v) <$> variable <*> ((throwaway Equal) *> assignment)

logicalOr :: Parser Expr
logicalOr = orOperation <|> logicalAnd
  where
    orOperation = (\x or y -> Logical x or y) <$> logicalAnd <*> (one Or) <*> logicalOr

logicalAnd :: Parser Expr
logicalAnd = andOperation <|> equality
  where
    andOperation = (\x and y -> Logical x and y) <$> equality <*> (one And) <*> logicalAnd

equality :: Parser Expr
equality = eqOperation <|> comparison
  where
    eqOperation = Binary <$> comparison <*> (oneOf [BangEqual, EqualEqual]) <*> equality

comparison :: Parser Expr
comparison = compOperation <|> term
  where
    compOperation = Binary <$> term <*> (oneOf [Greater, GreaterEqual, Less, LessEqual]) <*> comparison

term :: Parser Expr
term = termOperation <|> factor
  where
    termOperation = Binary <$> factor <*> (oneOf [Minus, Plus]) <*> term

factor :: Parser Expr
factor = factorOperation <|> unary
  where
    factorOperation = Binary <$> unary <*> (oneOf [Slash, Star]) <*> factor

unary :: Parser Expr
unary = unaryOperation <|> primary
  where
    unaryOperation = Unary <$> (oneOf [Bang, Minus]) <*> unary

primary :: Parser Expr
primary = number <|> string <|> true <|> false <|> nil <|> fullVariable <|> grouping
  where
    true  = convert $ TokenTrue  =#> (LiteralExpr $ BooleanLit True)
    false = convert $ TokenFalse =#> (LiteralExpr $ BooleanLit False)
    nil   = convert $        Nil =#>  LiteralExpr   NilLit

    grouping = Grouping <$> (throwaway LeftParen *> expr <* throwaway RightParen)

    number = parserFrom helper
      where
        helper tp@(TokenPlus (Number x) _) = win $ LiteralExpr (DoubleLit x) tp
        helper                          tp = backtrack [tp]

    string = parserFrom helper
      where
        helper tp@(TokenPlus (String s) _) = win $ LiteralExpr (StringLit s) tp
        helper                          tp = backtrack [tp]

fullVariable :: Parser Expr
fullVariable = (uncurry Variable) <$> variable
