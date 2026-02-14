module Lox.Parser.Internal.ParserError(
    ErrorPriority(Extreme, High, Low, Medium, Unimportant, VeryHigh)
  , ParserErrorType(Backtrack, ExpectedIdentifier, InvalidExpression, Missing, ReservedName, token, TooMuchArguing)
  , ParserError(offender, ParserError, prio, typ)
  ) where

import Lox.Scanner.Token(Token, TokenPlus)


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
  | ReservedName
  | ExpectedIdentifier
  | InvalidExpression
  | Missing { token :: Token }
  | TooMuchArguing
  deriving Eq

data ParserError =
  ParserError { typ :: ParserErrorType, prio :: ErrorPriority, offender :: TokenPlus }
