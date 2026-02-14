module Lox.Parser.Internal.Parse(
    (=#>), backtrack, convert, debug, errorWith, keywords, locOf, notFollowedBy, one, oneOf, Parsed, Parser(Parser, run), parserFrom, throwaway, variable, whineAbout, win
  ) where

import Control.Applicative(Alternative(empty))

import Lox.Scanner.Token(
    SourceLoc(SourceLoc)
  , Token(And, Class, Else, EOF, For, Fun, Identifier, If, Print, Return, Super, This, Var, While)
  , TokenPlus(loc, token, TokenPlus)
  )

import Lox.Parser.Internal.AST(Variable(Variable))

import Lox.Parser.Internal.ParserError(
    ErrorPriority(Unimportant, VeryHigh)
  , ParserError(ParserError, prio)
  , ParserErrorType(Backtrack, ReservedName)
  )


type Parsed = Either ParserError

newtype Parser a =
  Parser { run :: [TokenPlus] -> Parsed (a, [TokenPlus]) }
  deriving Functor

variable :: Parser Variable
variable = parserFrom helper
  where
    helper tp@(TokenPlus (Identifier s) _) = win $ Variable s tp
    helper tp@(TokenPlus t              _) =
      if not $ isReserved t then
        backtrack [tp]
      else
        errorWith $ ParserError ReservedName VeryHigh tp

throwaway :: Token -> Parser ()
throwaway token = convert $ token =#> (const ())

(=#>) :: Token -> a -> (Token, a)
(=#>) = (,)

convert :: (Token, TokenPlus -> a) -> Parser a
convert (t, mkResult) = parserFrom $ \tp -> if (tp.token == t) then win (mkResult tp) else backtrack [tp]

parserFrom :: (TokenPlus -> Parsed a) -> Parser a
parserFrom f = Parser $ parseOneToken f
  where
    parseOneToken _ []    = backtrack []
    parseOneToken f (h:t) = map (, t) $ f h

whineAbout :: ParserErrorType -> Parser a
whineAbout typ = parserFrom $ \t -> errorWith $ ParserError typ Unimportant t

oneOf :: [Token] -> Parser TokenPlus
oneOf = (map one) &> foldr (<|>) aempty

one :: Token -> Parser TokenPlus
one token = parserFrom $ \tp -> if (tp.token == token) then win tp else backtrack [tp]

locOf :: Token -> Parser SourceLoc
locOf token = map loc $ one token

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \tps -> runner tps $ p tps
  where
    runner tps (Left       _) = Right ((), tps)
    runner tps (Right (_, _)) = backtrack tps

win :: a -> Parsed a
win = Right

errorWith :: ParserError -> Parsed a
errorWith = Left

backtrack :: [TokenPlus] -> Parsed a
backtrack = listToMaybe &> (maybe dfault id) &> mkError &> errorWith
  where
    dfault  = TokenPlus EOF $ SourceLoc 0
    mkError = ParserError Backtrack Unimportant

isReserved :: Token -> Bool
isReserved = flip elem keywords

keywords :: [Token]
keywords = [And, Class, Else, For, Fun, If, Print, Return, Super, This, Var, While]

debug :: Parser ()
debug = Parser $ \tps -> win $ ((), traceShowId tps)

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
      helper (Left e1) (Left e2) = errorWith $ if e1.prio >= e2.prio then e1 else e2

instance Monad Parser where
  return          = pure
  Parser pa >>= f =
    Parser $ \tokens0 -> do
      (a, tokens1) <- pa tokens0
      (b, tokens2) <- (f a).run tokens1
      return (b, tokens2)
