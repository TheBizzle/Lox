module Lox.Evaluator.Internal.Program.ControlFlow(
    ControlFlow(exception, Exception, Normal, Return, value)
  ) where

import Lox.Scanner.Token(Token)

import Lox.Evaluator.Internal.Program.EvalError(EvalError)
import Lox.Evaluator.Internal.Program.Value(Value)


data ControlFlow
  = Exception {                 exception :: EvalError }
  | Normal    {                 value     ::     Value }
  | Return    { token :: Token, value     ::     Value }
  deriving Show
