module Lox.Verify.Internal.VerifierError(
    VerifierErrorType(CanOnlyRefSuperInsideClass, DuplicateVar, ThisClassHasNoSupers)
  , VerifierError(offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(TokenPlus)


data VerifierErrorType
  = CanOnlyRefSuperInsideClass
  | DuplicateVar
  | ThisClassHasNoSupers
  deriving (Eq, Show)

data VerifierError =
  VerifierError { typ :: VerifierErrorType, offender :: TokenPlus }
  deriving Show
