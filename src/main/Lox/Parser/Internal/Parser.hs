module Lox.Parser.Internal.Parser(
    (=#>), anyOf, atMost, bail, convert, cryAbout, debug, dummyToken, errorWith, multiError, notFollowedBy, one
  , Parsed(Backtrack, Errors, Parsed)
  , Parser(Parser, run)
  , parserFrom, require, throwaway, variable, win
  ) where

import Control.Applicative(Alternative(empty))

import Lox.Scanner.Token(SourceLoc(SourceLoc), Token(Token, typ), TokenType(EOF, Identifier))

import Lox.Parser.Internal.AST(Variable(Variable))

import Lox.Parser.Internal.ParserError(
    ParserError(ParserError)
  , ParserErrorType(Missing)
  )

import qualified Data.List.NonEmpty as NE


data Parsed a
  = Parsed a
  | Backtrack
  | Errors (NonEmpty ParserError)
  deriving Functor

newtype Parser a =
  Parser { run :: [Token] -> Parsed (a, [Token]) }
  deriving Functor

variable :: Parser Variable
variable =
  parserFrom $
    \token -> case token.typ of
      Identifier s -> win $ Variable s token
      _            -> bail

(=#>) :: TokenType -> a -> (TokenType, a)
(=#>) = (,)

dummyToken :: Token
dummyToken = Token EOF $ SourceLoc 0

parserFrom :: (Token -> Parsed a) -> Parser a
parserFrom f =
  Parser $ \case
    (h:t) -> map (, t) $ f h
    []    -> bail

cryAbout :: ParserErrorType -> Parser a
cryAbout typ =
  Parser $ \tokens ->
    errorWith $ ParserError typ $
      case tokens of
        (h:_) -> h
        []    -> dummyToken

multiError :: (NonEmpty ParserError) -> Parser a
multiError es = Parser $ const $ Errors es

anyOf :: [TokenType] -> Parser Token
anyOf tokens =
  parserFrom $
    \token ->
      if token.typ `elem` tokens then
        win token
      else
        bail

one :: TokenType -> Parser Token
one token = anyOf [token]

throwaway :: TokenType -> Parser ()
throwaway t = (const ()) <$> one t

convert :: (TokenType, Token -> a) -> Parser a
convert (t, mkResult) = mkResult <$> one t

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) =
  Parser $ \tokens ->
    case p tokens of
      Parsed  _ -> bail
      _         -> win ((), tokens)

require :: TokenType -> Parser ()
require x = throwaway x <|> cryAbout (Missing x)

atMost :: ParserErrorType -> Int -> (a -> Token) -> Parser a -> Parser [a]
atMost pet 0 f p =
  (do
    baddie <- p
    Parser $ const $ errorWith $ ParserError pet $ f baddie
  ) <|> pure []
atMost pet n f p = ((:) <$> p <*> atMost pet (n - 1) f p) <|> pure []

win :: a -> Parsed a
win = Parsed

bail :: Parsed a
bail = Backtrack

errorWith :: ParserError -> Parsed a
errorWith = NE.singleton &> Errors

debug :: Text -> Parser ()
debug label =
  Parser $ \tokens ->
    let msg = label <> ": " <> (showText tokens) in
    win ((), traceShow msg tokens)

instance Applicative Parser where
  pure x = Parser $ \ts -> Parsed (x, ts)

  Parser pf <*> Parser px =
    Parser $ \tokens0 ->
      case pf tokens0 of
        Errors es           -> Errors es
        Backtrack           -> Backtrack
        Parsed (f, tokens1) ->
          case px tokens1 of
            Errors es           -> Errors es
            Backtrack           -> Backtrack
            Parsed (x, tokens2) -> Parsed (f x, tokens2)

instance Alternative Parser where
  empty                 = Parser $ const Backtrack
  Parser p <|> Parser q = Parser $
    \tokens ->
      case p tokens of
        Errors es  -> Errors es
        Parsed res -> Parsed res
        Backtrack  -> q tokens

instance Monad Parser where
  return          = pure
  Parser pa >>= f =
    Parser $ \tokens0 ->
      case pa tokens0 of
        Errors e            -> Errors e
        Backtrack           -> Backtrack
        Parsed (a, tokens1) -> (f a).run tokens1
