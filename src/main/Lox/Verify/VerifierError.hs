{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Lox.Verify.VerifierError(module Lox.Verify.Internal.VerifierError) where

import Lox.Verify.Internal.VerifierError(
    VerifierError(offender, typ)
  , VerifierErrorType(CanOnlyRefSuperInsideClass, DuplicateVar, ThisClassHasNoSupers)
  )
