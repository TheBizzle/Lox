module Lox.Evaluator.Internal.Effect(Effect(output, Print)) where

data Effect
  = Print { output :: Text }
