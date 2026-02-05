module Lox.Evaluator.Internal.EvalError(
    EvalError(ArityMismatch, CanOnlyGetObj, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, CanOnlySetObj, ClassesCanOnlyContainFns, ClassNotFound, culprit, gotNum, NotAClass, NotCallable, NotImplemented, ObjectLacksKey, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, SuperCannotBeSelf, SuperMustBeAClass, ThisClassHasNoSupers, TopLevelReturn, UnknownVariable, varName, wantedNum)
  ) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.Value(Value)


data EvalError
  = ArityMismatch { culprit :: TokenPlus, wantedNum :: Word, gotNum :: Word }
  | CanOnlyGetObj { culprit :: TokenPlus }
  | CanOnlyRefSuperInsideClass { culprit :: TokenPlus }
  | CanOnlyRefThisInsideClass { culprit :: TokenPlus }
  | CanOnlySetObj { culprit :: TokenPlus }
  | ClassesCanOnlyContainFns { culprit :: TokenPlus }
  | ClassNotFound { culprit :: TokenPlus, className :: Text }
  | NotAClass { culprit :: TokenPlus, value :: Value }
  | NotCallable { culprit :: TokenPlus }
  | NotImplemented { culprit :: TokenPlus }
  | ObjectLacksKey { culprit :: TokenPlus, keyName :: Text }
  | OperandMustBeNumber { culprit :: TokenPlus }
  | OperandsMustBeNumbers { culprit :: TokenPlus }
  | OperandsMustBeNumsOrStrs { culprit :: TokenPlus }
  | SuperCannotBeSelf { culprit :: TokenPlus, className :: Text }
  | SuperMustBeAClass { culprit :: TokenPlus, className :: Text }
  | ThisClassHasNoSupers { culprit :: TokenPlus }
  | TopLevelReturn { culprit :: TokenPlus }
  | UnknownVariable { culprit :: TokenPlus, varName :: Text }
  deriving Show
