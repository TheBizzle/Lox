{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, errorWith, oneOf, Parsed, Parser(Parser, run), parserFrom, throwaway, whineAbout, win
  ) where

import Control.Applicative(Alternative(empty))

import Lox.Scanner.Token(
    Token(EOF),
    TokenPlus(lineNumber, token, TokenPlus)
  )

import Lox.Parser.Internal.ParserError(ParserError(ParserError), ParserErrorType(InvalidExpression))


type Parsed = Either ParserError

newtype Parser a =
  Parser { run :: [TokenPlus] -> Parsed (a, [TokenPlus]) }
  deriving Functor

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
