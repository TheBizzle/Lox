module Lox.Evaluator.Internal.World(World, WorldState(bindings, effects, WorldState)) where

import Control.Monad.State(State)

import Lox.Evaluator.Internal.Effect(Effect)
import Lox.Evaluator.Internal.Value(Value)


type World = State WorldState

data WorldState
  = WorldState { bindings :: Map Text Value, effects :: [Effect] }
