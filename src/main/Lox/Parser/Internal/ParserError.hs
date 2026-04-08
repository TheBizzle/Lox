module Lox.Parser.Internal.ParserError(
    ParserError(offender, ParserError, typ)
  , ParserErrorType(ExpectedBraceBeforeBody, ExpectedDotAfterSuper, ExpectedIdentifier, ExpectedParenAfterArgs, ExpectedParenAfterParams, ExpectedPropertyName, ExpectedSuperMethodName, ExpectedSuperName, Incomprehensible, InvalidAssign, InvalidExpression, Missing, ReservedName, token, TooMuchArguing, TooMuchParaming, UnfinishedStmt)
  ) where

import Lox.Scanner.Token(Token, TokenPlus)


data ParserErrorType
  = ExpectedBraceBeforeBody
  | ExpectedDotAfterSuper
  | ExpectedIdentifier
  | ExpectedParenAfterArgs
  | ExpectedParenAfterParams
  | ExpectedPropertyName
  | ExpectedSuperMethodName
  | ExpectedSuperName
  | Incomprehensible
  | InvalidAssign
  | InvalidExpression
  | Missing { token :: Token }
  | ReservedName
  | TooMuchArguing
  | TooMuchParaming
  | UnfinishedStmt
  deriving (Eq, Show)

data ParserError =
  ParserError { typ :: ParserErrorType, offender :: TokenPlus }
  deriving Show
