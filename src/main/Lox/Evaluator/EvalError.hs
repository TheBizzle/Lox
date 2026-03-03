{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Lox.Evaluator.EvalError(module Lox.Evaluator.Internal.EvalError) where

import Lox.Evaluator.Internal.EvalError(
    EvalError(culprit, EvalError, typ)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlySetObj, ClassNotFound, gotNum, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperMustBeAClass, TopLevelReturn, UnknownVariable, varName, wantedNum)
  )
