module Lox.Evaluator.Internal.EvalError(
    EvalError(ArityMismatch, culprit, gotNum, NotCallable, NotImplemented, mismatches, operator, TopLevelReturn, TypeError, UnknownVariable, varName, wantedNum)
  ) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.Type(Type)
import Lox.Evaluator.Internal.Value(Value)


data EvalError
  = ArityMismatch { culprit :: TokenPlus, wantedNum :: Word, gotNum :: Word }
  | NotCallable { culprit :: TokenPlus }
  | NotImplemented { culprit :: TokenPlus }
  | TopLevelReturn
  | TypeError { operator :: TokenPlus, mismatches :: [(Type, Value)] }
  | UnknownVariable { culprit :: TokenPlus, varName :: Text }
  deriving Show
