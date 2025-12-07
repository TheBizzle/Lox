{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Parser.Internal.Parser(parse) where

import Control.Applicative(many)
import Control.Lens((#))

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(_Failure, _Success, Validation)

import Lox.Scanner.Token(Token(EOF, Print, Semicolon), TokenPlus)

import Lox.Parser.Internal.ExpressionParser(expression, run)
import Lox.Parser.Internal.Parse(throwaway)
import Lox.Parser.Internal.ParserError(ParserError)
import Lox.Parser.Internal.Program(Program(Program))

import qualified Data.List.NonEmpty as NE


parse :: [TokenPlus] -> Validation (NonEmpty ParserError) Program
parse = expression.run &> (either handleError handleSuccess)
  where
    handleError   = NE.singleton &> (_Failure #)
    handleSuccess = fst &> Program &> (_Success #)
