{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Parser.Internal.Parser(parse) where

import Control.Applicative(many)
import Control.Lens((#))

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(_Failure, _Success, Validation)

import Lox.Scanner.Token(Token(EOF, Print, Semicolon), TokenPlus)

import Lox.Parser.Internal.ExpressionParser(expression, run)
import Lox.Parser.Internal.Parse(one, Parser, throwaway, whineAbout)
import Lox.Parser.Internal.ParserError(ParserError, ParserErrorType(InvalidStatement, MissingSemicolon))
import Lox.Parser.Internal.Program(Program(Program), Statement(ExpressionStatement, PrintStatement))

import qualified Data.List.NonEmpty as NE


parse :: [TokenPlus] -> Validation (NonEmpty ParserError) Program
parse = parser.run &> (either handleError handleSuccess)
  where
    handleError   = NE.singleton &> (_Failure #)
    handleSuccess = fst &> (_Success #)

parser :: Parser Program
parser = program

program :: Parser Program
program = goodProgram <|> badProgram
  where
    goodProgram = Program <$> (many statement) <* (throwaway EOF)

    badProgram   = (many statement) *> badStatement
    badStatement = expression *> whineAbout MissingSemicolon

statement :: Parser Statement
statement = printStatement <|> exprStatement

exprStatement :: Parser Statement
exprStatement = ExpressionStatement <$> (expression <* (throwaway Semicolon))

printStatement :: Parser Statement
printStatement = PrintStatement <$> (one Print) <*> (expression <* (throwaway Semicolon))
