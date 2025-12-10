{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Evaluator.Internal.World(cleanupScope, empty, getVar, initScope, setVar, World, WorldState(scopes, effects, WorldState)) where

import Control.Monad.State(State)

import Data.Word(Word)
import Data.List.NonEmpty((<|), NonEmpty)
import Data.Map(lookup, union)

import Lox.Evaluator.Internal.Effect(Effect)
import Lox.Evaluator.Internal.Value(Value)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set


type World = State WorldState

-- I think we'll need to address scopes and variables like this, in order to support setting closed-over variables.
-- --Jason B. (12/9/25)
newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord)

data VarAddress
  = VarAddress Text ScopeAddress
  deriving (Eq, Ord)

data WorldState
  = WorldState { variables :: Map VarAddress Value, scopes :: NonEmpty Scope
               , lastScopeAddr :: ScopeAddress, effects :: [Effect] }

data Scope
  = Scope { locals :: Set Text, address :: ScopeAddress }

initScope :: WorldState -> WorldState
initScope world = world { scopes = newScope <| world.scopes, lastScopeAddr = newAddr }
  where
    newAddr  = ScopeAddress $ world.lastScopeAddr.n + 1
    newScope = Scope Set.empty newAddr

cleanupScope :: WorldState -> ScopeAddress -> WorldState
cleanupScope world addr = world { variables = newVariables }
  where
    newVariables = Map.filterWithKey (\(VarAddress _ vsa) _ -> vsa /= addr) world.variables

empty :: WorldState
empty =
    WorldState {
      variables     = Map.empty,
      scopes        = (NE.singleton $ Scope Set.empty defaultAddr),
      lastScopeAddr = defaultAddr,
      effects       = []
    }
  where
    defaultAddr = ScopeAddress 0

getVar :: Text -> WorldState -> Maybe Value
getVar varName state = state |> getAddr &> getValue
  where
    getAddr            = scopes &> (foldr concatVarMap Map.empty) &> (lookup varName)
    concatVarMap x acc = x |> locals &> Set.toList &> map (\n -> (n, VarAddress n x.address)) &>
                              Map.fromList &> (flip union acc)
    getValue           = (>>= (flip lookup state.variables))

setVar :: Text -> Value -> WorldState -> WorldState
setVar varName value state = state { variables = newVars, scopes = newScopes }
  where
    (h, tM)   = NE.uncons state.scopes
    newH      = h { locals = Set.insert varName h.locals }
    newScopes = maybe (NE.singleton newH) (NE.cons newH) tM
    newVars   = Map.insert (VarAddress varName h.address) value state.variables
