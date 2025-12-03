module Lox.Parser.Internal.ParserError(
    ParserErrorType(InvalidExpression, MissingClosingParen)
  , ParserError(lineNumber, offender, ParserError, typ)
  ) where

import Lox.Scanner.Token(Token)


data ParserErrorType
  = InvalidExpression
  | MissingClosingParen

data ParserError =
  ParserError { typ :: ParserErrorType, lineNumber :: Int, offender :: Token }
