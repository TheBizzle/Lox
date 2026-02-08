module Lox.Parser.Internal.ExpressionParser(expression, unary) where

import Lox.Scanner.Token(
    Token(And, Bang, BangEqual, Comma, Dot, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Or, Plus, RightParen, This, TokenFalse, TokenTrue, Slash, Super, Star, String)
  , TokenPlus(TokenPlus)
  )

import Lox.Parser.Internal.AST(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Unary, VarRef)
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Variable
  )

import Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, notFollowedBy, one, oneOf, Parser, parserFrom, throwaway, variable, win, whineAbout
  )

import Lox.Parser.Internal.ParserError(ParserErrorType(TooMuchArguing))

import qualified Lox.Parser.Internal.AST as AST


expression :: Parser Expr
expression = expr

expr :: Parser Expr
expr = assignment

assignment :: Parser Expr
assignment = setOperation <|> assignOperation <|> logicalOr
  where
    assignOperation =
      Assign <$> variable <*> ((throwaway Equal) *> assignment)

    setOperation =
      Set <$> (fnCall <* (throwaway Dot)) <*> variable <*> ((throwaway Equal) *> assignment)

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
unary = unaryOperation <|> fnCall
  where
    unaryOperation = Unary <$> (oneOf [Bang, Minus]) <*> unary

data CallType
  = Simple TokenPlus [Expr]
  | Method Variable

fnCall :: Parser Expr
fnCall = call <|> primary
  where
    call       = makeCalls <$> primary <*> (some invocation)
    invocation = simpleCall <|> getMethod

    simpleCall = Simple <$> (one LeftParen) <*> (args <* (throwaway RightParen))
    getMethod  = Method <$> ((throwaway Dot) *> variable) <* (notFollowedBy $ one Equal)

    args       = (limited existent) <|> nullary
    existent   = (:) <$> expression <*> (many $ (throwaway Comma) *> expression)
    nullary    = pure []
    limited p  = p >>= (\args -> if (length args) < 255 then return args else whineAbout TooMuchArguing)

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

    grouping = Grouping <$> (throwaway LeftParen *> expr <* throwaway RightParen)

    super = AST.Super <$> (one Super) <*> (throwaway Dot *> variable)

    number = parserFrom helper
      where
        helper tp@(TokenPlus (Number x) _) = win $ LiteralExpr (DoubleLit x) tp
        helper                          tp = backtrack [tp]

    string = parserFrom helper
      where
        helper tp@(TokenPlus (String s) _) = win $ LiteralExpr (StringLit s) tp
        helper                          tp = backtrack [tp]

fullVariable :: Parser Expr
fullVariable = VarRef <$> variable
