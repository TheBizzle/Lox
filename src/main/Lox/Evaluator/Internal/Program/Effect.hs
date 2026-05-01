module Lox.Evaluator.Internal.Program.Effect(Effect(output, Print), runEffect) where

import Lox.Evaluator.Internal.Program.Program(Evaluated)
import Lox.Evaluator.Internal.Program.Value(Value(Nada))

import qualified Data.Text.IO as TIO


runEffect :: Effect -> Evaluated
runEffect (Print x) = (liftIO $ TIO.putStrLn x) $> (Success Nada)

data Effect
  = Print { output :: Text }
  deriving Show
