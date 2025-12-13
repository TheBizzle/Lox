module Lox.Parser.Internal.ExpressionParser(expression, unary) where

import Lox.Scanner.Token(
    Token(Bang, BangEqual, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Plus, RightParen, TokenFalse, TokenTrue, Slash, Star, String),
    TokenPlus(TokenPlus)
  )

import Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, oneOf, Parser, parserFrom, throwaway, variable, win
  )

import Lox.Parser.Internal.Program(
    Expr(Binary, Grouping, LiteralExpr, Unary),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  )


expression :: Parser Expr
expression = expr

expr :: Parser Expr
expr = equality

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
primary = number <|> string <|> true <|> false <|> nil <|> variable <|> grouping
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
