module Lox.Evaluator.Internal.EvalError(
    EvalError(ArityMismatch, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, ClassesCanOnlyContainFns, ClassNotFound, culprit, gotNum, NotAClass, NotAnObject, NotCallable, NotImplemented, mismatches, ObjectLacksKey, operator, SuperCannotBeSelf, SuperMustBeAClass, ThisClassHasNoSupers, TopLevelReturn, TypeError, UnknownVariable, varName, wantedNum)
  ) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.Type(Type)
import Lox.Evaluator.Internal.Value(Value)


data EvalError
  = ArityMismatch { culprit :: TokenPlus, wantedNum :: Word, gotNum :: Word }
  | CanOnlyRefSuperInsideClass { culprit :: TokenPlus }
  | CanOnlyRefThisInsideClass { culprit :: TokenPlus }
  | ClassesCanOnlyContainFns { culprit :: TokenPlus }
  | ClassNotFound { className :: Text }
  | NotAClass { value :: Value }
  | NotAnObject { value :: Value }
  | NotCallable { culprit :: TokenPlus }
  | NotImplemented { culprit :: TokenPlus }
  | ObjectLacksKey { keyName :: Text }
  | SuperCannotBeSelf { culprit :: TokenPlus, className :: Text }
  | SuperMustBeAClass { culprit :: TokenPlus, className :: Text }
  | ThisClassHasNoSupers { culprit :: TokenPlus }
  | TopLevelReturn
  | TypeError { operator :: TokenPlus, mismatches :: [(Type, Value)] }
  | UnknownVariable { culprit :: TokenPlus, varName :: Text }
  deriving Show
