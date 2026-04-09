module Lox.Parser.Internal.Parser(
    (=#>), anyOf, atMost, bail, convert, cryAbout, debug, dummyTP, errorWith, multiError, notFollowedBy, one
  , Parsed(Backtrack, Errors, Parsed)
  , Parser(Parser, run)
  , parserFrom, require, throwaway, variable, win
  ) where

import Control.Applicative(Alternative(empty))

import Lox.Scanner.Token(SourceLoc(SourceLoc), Token(EOF, Identifier), TokenPlus(token, TokenPlus))

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
  Parser { run :: [TokenPlus] -> Parsed (a, [TokenPlus]) }
  deriving Functor

variable :: Parser Variable
variable =
  parserFrom $
    \tp -> case tp.token of
      Identifier s -> win $ Variable s tp
      _            -> bail

(=#>) :: Token -> a -> (Token, a)
(=#>) = (,)

dummyTP :: TokenPlus
dummyTP = TokenPlus EOF $ SourceLoc 0

parserFrom :: (TokenPlus -> Parsed a) -> Parser a
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
        []    -> dummyTP

multiError :: (NonEmpty ParserError) -> Parser a
multiError es = Parser $ const $ Errors es

anyOf :: [Token] -> Parser TokenPlus
anyOf tokens =
  parserFrom $
    \tp ->
      if tp.token `elem` tokens then
        win tp
      else
        bail

one :: Token -> Parser TokenPlus
one token = anyOf [token]

throwaway :: Token -> Parser ()
throwaway t = (const ()) <$> one t

convert :: (Token, TokenPlus -> a) -> Parser a
convert (t, mkResult) = mkResult <$> one t

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) =
  Parser $ \tps ->
    case p tps of
      Parsed  _ -> bail
      _         -> win ((), tps)

require :: Token -> Parser ()
require x = throwaway x <|> cryAbout (Missing x)

atMost :: ParserErrorType -> Int -> (a -> TokenPlus) -> Parser a -> Parser [a]
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
  Parser $ \tps ->
    let msg = label <> ": " <> (showText tps) in
    win ((), traceShow msg tps)

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
