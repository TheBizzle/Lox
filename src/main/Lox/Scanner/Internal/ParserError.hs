module Lox.Scanner.Internal.ParserError(
    ParserErrorType(InvalidNumberFormat, UnknownToken, UnterminatedString)
  , ParserError(lineNumber, ParserError, typ)
  ) where

data ParserErrorType
  = InvalidNumberFormat Text
  | UnknownToken Text
  | UnterminatedString

data ParserError =
  ParserError { typ :: ParserErrorType, lineNumber :: Int }
