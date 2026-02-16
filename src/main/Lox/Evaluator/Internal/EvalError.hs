module Lox.Evaluator.Internal.EvalError(
    EvalError(culprit, EvalError, typ)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlyRefThisInsideClass, CanOnlySetObj, ClassNotFound, gotNum, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperCannotBeSelf, SuperMustBeAClass, TopLevelReturn, UnknownVariable, varName, wantedNum)
  ) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.Value(Value)


data EvalErrorType
  = ArityMismatch { wantedNum :: Word, gotNum :: Word }
  | CanOnlyGetObj
  | CanOnlyRefThisInsideClass
  | CanOnlySetObj
  | ClassNotFound { className :: Text }
  | NotAClass { value :: Value }
  | NotCallable
  | NotImplemented
  | ObjectLacksKey { keyName :: Text }
  | OperandMustBeNumber
  | OperandsMustBeNumbers
  | OperandsMustBeNumsOrStrs
  | SuperCannotBeSelf { className :: Text }
  | SuperMustBeAClass { className :: Text }
  | TopLevelReturn
  | UnknownVariable { varName :: Text }
  deriving Show

data EvalError
  = EvalError { typ :: EvalErrorType, culprit :: TokenPlus }
  deriving Show
