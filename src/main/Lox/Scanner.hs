module Lox.Scanner(ParserError(errorType, lineNumber, ParserError), scan, Token) where

import Control.Lens((#))

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(_Success, Validation)

scan :: Text -> ParserResult [Token]
scan _ = _Success # []

type ParserResult a = Validation (NonEmpty ParserError) a

data ParserErrorType

data ParserError =
  ParserError { errorType :: ParserErrorType, lineNumber :: Int }

data Token
  = Any
