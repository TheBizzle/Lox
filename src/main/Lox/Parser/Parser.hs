{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Lox.Parser.Parser(parse) where

import Control.Applicative(Alternative(empty))
import Control.Lens((#))

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(_Failure, _Success, Validation)

import Lox.Scanner.Token(
    Token(Bang, BangEqual, EOF, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Nil, Number, Plus, RightParen, TokenFalse, TokenTrue, Slash, Star, String),
    TokenPlus(lineNumber, token, TokenPlus)
  )

import Lox.Parser.Internal.ParserError(ParserError(ParserError), ParserErrorType(InvalidExpression, MissingClosingParen))

import Lox.Parser.Internal.Program(
    Expr(Binary, Grouping, LiteralExpr, Unary),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(Program)
  )

import qualified Data.List.NonEmpty as NE


type Parsed = Either ParserError

newtype Parser a =
  Parser { run :: [TokenPlus] -> Parsed (a, [TokenPlus]) }
  deriving Functor

type LoxParser = Parser Expr

parse :: [TokenPlus] -> Validation (NonEmpty ParserError) Program
parse = expr.run &> (either handleError handleSuccess)
  where
    handleError   = NE.singleton &> (_Failure #)
    handleSuccess = fst &> Program &> (_Success #)

expr :: LoxParser
expr = equality

equality :: LoxParser
equality = eqOperation <|> comparison
  where
    eqOperation = Binary <$> comparison <*> (oneOf [BangEqual, EqualEqual]) <*> equality

comparison :: LoxParser
comparison = compOperation <|> term
  where
    compOperation = Binary <$> term <*> (oneOf [Greater, GreaterEqual, Less, LessEqual]) <*> comparison

term :: LoxParser
term = termOperation <|> factor
  where
    termOperation = Binary <$> factor <*> (oneOf [Minus, Plus]) <*> term

factor :: LoxParser
factor = factorOperation <|> unary
  where
    factorOperation = Binary <$> unary <*> (oneOf [Slash, Star]) <*> factor

unary :: LoxParser
unary = unaryOperation <|> primary
  where
    unaryOperation = Unary <$> (oneOf [Bang, Minus]) <*> unary

primary :: LoxParser
primary = number <|> string <|> true <|> false <|> nil <|> grouping
  where
    true  = convert $ TokenTrue  =#> (LiteralExpr $ BooleanLit True)
    false = convert $ TokenFalse =#> (LiteralExpr $ BooleanLit False)
    nil   = convert $        Nil =#>  LiteralExpr   NilLit

    grouping = validGrouping <|> whineAboutGrouping
      where
        validGrouping      = Grouping <$> (throwaway LeftParen *> expr <* throwaway RightParen)
        whineAboutGrouping = throwaway LeftParen *> expr *> whineAbout MissingClosingParen

    number = parserFrom helper
      where
        helper (TokenPlus (Number x) _) = win $ LiteralExpr $ DoubleLit x
        helper                       tp = backtrack [tp]

    string = parserFrom helper
      where
        helper (TokenPlus (String s) _) = win $ LiteralExpr $ StringLit s
        helper                       tp = backtrack [tp]

throwaway :: Token -> Parser ()
throwaway token = convert $ token =#> ()

(=#>) :: Token -> a -> (Token, a)
(=#>) = (,)

convert :: (Token, a) -> Parser a
convert (t, result) = parserFrom $ \tp -> if (tp.token == t) then win result else backtrack [tp]

parserFrom :: (TokenPlus -> Parsed a) -> Parser a
parserFrom f = Parser $ parseOneToken f
  where
    parseOneToken _ []    = backtrack []
    parseOneToken f (h:t) = map (, t) $ f h

whineAbout :: ParserErrorType -> Parser a
whineAbout typ = parserFrom $ \t -> errorWith $ ParserError typ t.lineNumber t.token

oneOf :: [Token] -> Parser TokenPlus
oneOf = (map one) &> foldr (<|>) empty

one :: Token -> Parser TokenPlus
one token = parserFrom $ \tp -> if (tp.token == token) then win tp else backtrack [tp]

win :: a -> Parsed a
win = Right

errorWith :: ParserError -> Parsed a
errorWith = Left

backtrack :: [TokenPlus] -> Parsed a
backtrack = listToMaybe &> (maybe dfault id) &> (lineNumber &&& token) &> (uncurry mkError) &> errorWith
  where
    dfault  = TokenPlus EOF 0
    mkError = ParserError InvalidExpression

instance Applicative Parser where
  pure x = Parser $ \ts -> win (x, ts)

  Parser pf <*> Parser px =
    Parser $ \tokens0 -> do
      (f, tokens1) <- pf tokens0
      (x, tokens2) <- px tokens1
      return $ (f x, tokens2)

instance Alternative Parser where
  empty                 = Parser backtrack
  Parser p <|> Parser q = Parser $ \ts -> helper (p ts) (q ts)
    where
      -- Valid parse > real error > backtrack --Jason B. (12/5/25)
      helper (Right r)         _ = win r
      helper         _ (Right r) = win r
      helper (Left e1) (Left e2) = errorWith $ betterError e1 e2

      betterError (ParserError InvalidExpression _ _) e2 = e2
      betterError                                  e1  _ = e1
