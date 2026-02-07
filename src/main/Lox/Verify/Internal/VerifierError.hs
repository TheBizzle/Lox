module Lox.Verify.Internal.VerifierError(
    VerifierErrorType()
  , VerifierError(lineNumber, offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(Token)

data VerifierErrorType
  deriving Eq

data VerifierError =
  VerifierError { typ :: VerifierErrorType, lineNumber :: Word, offender :: Token }
