module Lox.Parser.Internal.ParserError(
    ParserError(offender, ParserError, typ)
  , ParserErrorType(ExpectedBraceBeforeBody, ExpectedDotAfterSuper, ExpectedIdentifier, ExpectedParenAfterArgs, ExpectedParenAfterParams, ExpectedPropertyName, ExpectedSuperMethodName, ExpectedSuperName, Incomprehensible, InvalidAssign, InvalidExpression, Missing, ReservedName, tokenType, TooMuchArguing, TooMuchParaming, UnfinishedStmt)
  ) where

import Lox.Scanner.Token(Token, TokenType)


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
  | Missing { tokenType :: TokenType }
  | ReservedName
  | TooMuchArguing
  | TooMuchParaming
  | UnfinishedStmt
  deriving (Eq, Show)

data ParserError =
  ParserError { typ :: ParserErrorType, offender :: Token }
  deriving Show
