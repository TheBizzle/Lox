module Lox.Evaluator.Internal.ControlFlow(ControlFlow(exception, Exception, Normal, Return, value)) where

import Lox.Scanner.Token(TokenPlus)

import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Value(Value)

data ControlFlow
  = Exception {                     exception :: EvalError }
  | Normal    {                     value     ::     Value }
  | Return    { token :: TokenPlus, value     ::     Value }
  deriving Show
