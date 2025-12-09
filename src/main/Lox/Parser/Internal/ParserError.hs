module Lox.Parser.Internal.ParserError(
    ParserErrorType(Backtrack, InvalidExpression, InvalidStatement, MissingClosingParen, MissingSemicolon)
  , ParserError(lineNumber, offender, ParserError, typ)
  ) where

import Lox.Scanner.Token(Token)


-- Do NOT arbitrarily reorder this.  The ordering is used for the `Alternative` instance. --Jason B. (12/7/25)
data ParserErrorType
  = Backtrack
  | InvalidStatement
  | MissingSemicolon
  | InvalidExpression
  | MissingClosingParen
  deriving (Eq, Ord)

data ParserError =
  ParserError { typ :: ParserErrorType, lineNumber :: Int, offender :: Token }
