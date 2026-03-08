{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Lox.Verifier.VerifierError(module Lox.Verifier.Internal.VerifierError) where

import Lox.Verifier.Internal.VerifierError(
    VerifierError(offender, typ)
  , VerifierErrorType(CannotInheritFromSelf, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers, VarCannotInitInTermsOfSelf)
  )
