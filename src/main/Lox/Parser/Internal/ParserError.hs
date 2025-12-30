module Lox.Parser.Internal.ParserError(
    ErrorPriority(Extreme, High, Low, Medium, Unimportant, VeryHigh)
  , ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing, ReservedName, token, TooMuchArguing)
  , ParserError(lineNumber, offender, ParserError, prio, typ)
  ) where

import Lox.Scanner.Token(Token)


data ErrorPriority
  = Unimportant
  | Low
  | Medium
  | High
  | VeryHigh
  | Extreme
  deriving (Eq, Ord)

data ParserErrorType
  = Backtrack
  | ReservedName { token :: Token }
  | ExpectedIdentifier
  | InvalidExpression
  | Missing { token :: Token }
  | TooMuchArguing
  deriving Eq

data ParserError =
  ParserError { typ :: ParserErrorType, prio :: ErrorPriority, lineNumber :: Word, offender :: Token }
