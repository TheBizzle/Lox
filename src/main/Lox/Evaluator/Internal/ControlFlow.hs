module Lox.Evaluator.Internal.ControlFlow(ControlFlow(exception, Exception, Normal, Return, value)) where

import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Value(Value)

data ControlFlow
  = Exception { exception :: EvalError }
  | Normal    { value     ::     Value }
  | Return    { value     ::     Value }
  deriving Show
