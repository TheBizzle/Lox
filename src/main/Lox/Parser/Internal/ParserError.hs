module Lox.Parser.Internal.ParserError(
    ErrorPriority(Extreme, High, Low, Medium, Unimportant, VeryHigh)
  , ParserError(offender, ParserError, prio, typ)
  , ParserErrorType(Backtrack, ExpectedBraceBeforeBody, ExpectedDotAfterSuper, ExpectedIdentifier, ExpectedParenAfterParams, ExpectedPropertyName, ExpectedSuperMethodName, ExpectedSuperName, InvalidAssign, InvalidExpression, Missing, ReservedName, token, TooMuchArguing, TooMuchParaming, UnfinishedStmt)
  ) where

import Lox.Scanner.Token(Token, TokenPlus)


data ErrorPriority
  = Unimportant
  | Low
  | Medium
  | High
  | VeryHigh
  | Extreme
  deriving (Eq, Ord, Show)

data ParserErrorType
  = Backtrack
  | ReservedName
  | ExpectedBraceBeforeBody
  | ExpectedDotAfterSuper
  | ExpectedIdentifier
  | ExpectedParenAfterParams
  | ExpectedPropertyName
  | ExpectedSuperMethodName
  | ExpectedSuperName
  | InvalidAssign
  | InvalidExpression
  | Missing { token :: Token }
  | TooMuchArguing
  | TooMuchParaming
  | UnfinishedStmt
  deriving (Eq, Show)

data ParserError =
  ParserError { typ :: ParserErrorType, prio :: ErrorPriority, offender :: TokenPlus }
  deriving Show
