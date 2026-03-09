module Lox.Verifier.Internal.VerifierError(
    VerifierErrorType(CannotInheritFromSelf, CannotReturnInConstructor, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers, VarCannotInitInTermsOfSelf)
  , VerifierError(offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(TokenPlus)


data VerifierErrorType
  = CannotInheritFromSelf
  | CannotReturnInConstructor
  | CanOnlyRefSuperInsideClass
  | CanOnlyRefThisInsideClass
  | DuplicateVar
  | ThisClassHasNoSupers
  | VarCannotInitInTermsOfSelf
  deriving (Eq, Show)

data VerifierError =
  VerifierError { typ :: VerifierErrorType, offender :: TokenPlus }
  deriving Show
