module Lox.Parser.Internal.ParserError(
    ErrorPriority(Extreme, High, Low, Medium, Unimportant, VeryHigh)
  , ParserError(offender, ParserError, prio, typ)
  , ParserErrorType(Backtrack, ExpectedDotAfterSuper, ExpectedIdentifier, ExpectedParenAfterParams, ExpectedPropertyName, ExpectedSuperMethodName, ExpectedSuperName, InvalidExpression, Missing, ReservedName, token, TooMuchArguing, TooMuchParaming)
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
  | ExpectedDotAfterSuper
  | ExpectedIdentifier
  | ExpectedParenAfterParams
  | ExpectedPropertyName
  | ExpectedSuperMethodName
  | ExpectedSuperName
  | InvalidExpression
  | Missing { token :: Token }
  | TooMuchArguing
  | TooMuchParaming
  deriving Eq

data ParserError =
  ParserError { typ :: ParserErrorType, prio :: ErrorPriority, offender :: TokenPlus }
