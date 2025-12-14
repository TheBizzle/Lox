module Lox.Evaluator.Internal.EvalError(
    EvalError(culprit, NotImplemented, mismatches, operator, TypeError, UnknownVariable, varName)
  ) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.Type(Type)
import Lox.Evaluator.Internal.Value(Value)


data EvalError
  = NotImplemented { culprit :: TokenPlus }
  | TypeError { operator :: TokenPlus, mismatches :: [(Type, Value)] }
  | UnknownVariable { culprit :: TokenPlus, varName :: Text }
  deriving Show
