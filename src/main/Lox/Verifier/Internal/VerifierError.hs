module Lox.Verifier.Internal.VerifierError(
    VerifierErrorType(CannotInheritFromSelf, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers)
  , VerifierError(offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(TokenPlus)


data VerifierErrorType
  = CannotInheritFromSelf
  | CanOnlyRefSuperInsideClass
  | CanOnlyRefThisInsideClass
  | DuplicateVar
  | ThisClassHasNoSupers
  deriving (Eq, Show)

data VerifierError =
  VerifierError { typ :: VerifierErrorType, offender :: TokenPlus }
  deriving Show
