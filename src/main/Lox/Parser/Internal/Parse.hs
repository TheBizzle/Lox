module Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, errorWith, one, oneOf, Parsed, Parser(Parser, run), parserFrom, throwaway, variable, whineAbout, win
  ) where

import Control.Applicative(Alternative(empty))

import Lox.Scanner.Token(
    Token(EOF, Identifier),
    TokenPlus(lineNumber, token, TokenPlus)
  )

import Lox.Parser.Internal.ParserError(
    ParserError(ParserError, typ),
    ParserErrorType(Backtrack)
  )

import Lox.Parser.Internal.Program(Expr(Variable))


type Parsed = Either ParserError

newtype Parser a =
  Parser { run :: [TokenPlus] -> Parsed (a, [TokenPlus]) }
  deriving Functor

variable :: Parser Expr
variable = parserFrom helper
  where
    helper tp@(TokenPlus (Identifier _) _) = win $ Variable tp
    helper                              tp = backtrack [tp]

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
oneOf = (map one) &> foldr (<|>) aempty

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
    mkError = ParserError Backtrack

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
      helper (Right r)         _ = win r
      helper         _ (Right r) = win r
      helper (Left e1) (Left e2) = errorWith $ if e1.typ >= e2.typ then e1 else e2
