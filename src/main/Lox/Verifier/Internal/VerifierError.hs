module Lox.Verifier.Internal.VerifierError(
    VerifierErrorType(CannotInheritFromSelf, CannotReturnAtTopLevel, CannotReturnInConstructor, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers, VarCannotInitInTermsOfSelf)
  , VerifierError(offender, VerifierError, typ)
  ) where

import Lox.Scanner.Token(Token)


data VerifierErrorType
  = CannotInheritFromSelf
  | CannotReturnAtTopLevel
  | CannotReturnInConstructor
  | CanOnlyRefSuperInsideClass
  | CanOnlyRefThisInsideClass
  | DuplicateVar
  | ThisClassHasNoSupers
  | VarCannotInitInTermsOfSelf
  deriving (Eq, Show)

data VerifierError =
  VerifierError { typ :: VerifierErrorType, offender :: Token }
  deriving Show
