module Lox.Evaluator.Internal.World(empty, World, WorldState(bindings, effects, WorldState)) where

import Control.Monad.State(State)

import Lox.Evaluator.Internal.Effect(Effect)
import Lox.Evaluator.Internal.Value(Value)

import qualified Data.Map as Map


type World = State WorldState

data WorldState
  = WorldState { bindings :: Map Text Value, effects :: [Effect] }

empty = WorldState Map.empty []
