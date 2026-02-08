module Lox.Verify.Internal.VerifierError(
    VerifierErrorType(DuplicateVar)
  , VerifierError(offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(TokenPlus)


data VerifierErrorType
  = DuplicateVar
  deriving Eq

data VerifierError =
  VerifierError { typ :: VerifierErrorType, offender :: TokenPlus }
