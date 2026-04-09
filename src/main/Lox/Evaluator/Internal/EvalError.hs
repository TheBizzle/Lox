module Lox.Evaluator.Internal.EvalError(
    EvalError(culprit, EvalError, typ)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlySetObj, ClassNotFound, gotNum, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperMustBeAClass, TopLevelReturn, UnknownVariable, varName, wantedNum)
  ) where

import Lox.Scanner.Token(Token)

import Lox.Evaluator.Internal.Value(Value)


data EvalErrorType
  = ArityMismatch { wantedNum :: Word, gotNum :: Word }
  | CanOnlyGetObj
  | CanOnlySetObj
  | ClassNotFound { className :: Text }
  | NotAClass { value :: Value }
  | NotCallable
  | NotImplemented
  | ObjectLacksKey { keyName :: Text }
  | OperandMustBeNumber
  | OperandsMustBeNumbers
  | OperandsMustBeNumsOrStrs
  | SuperMustBeAClass { className :: Text }
  | TopLevelReturn
  | UnknownVariable { varName :: Text }
  deriving Show

data EvalError
  = EvalError { typ :: EvalErrorType, culprit :: Token }
  deriving Show
