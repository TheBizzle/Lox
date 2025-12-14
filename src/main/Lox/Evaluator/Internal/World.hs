module Lox.Evaluator.Internal.World(closeOver, declareVar, empty, getVar, popScope, pushScope, setVar, World, WorldState(scopes, effects, WorldState)) where

import Data.List.NonEmpty((<|))
import Data.Map(alter, lookup, union)

import Lox.Evaluator.Internal.Effect(Effect)
import Lox.Evaluator.Internal.Value(Value)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set


type World = State WorldState

newtype ScopeAddress
  = ScopeAddress { n :: Word }
  deriving (Eq, Ord, Show)

data VarAddress
  = VarAddress Text ScopeAddress
  deriving (Eq, Ord, Show)

data WorldState
  = WorldState { variables :: Map VarAddress Value, closures :: Map VarAddress (Set ScopeAddress)
               , scopes :: NonEmpty Scope, lastScopeAddr :: ScopeAddress
               , effects :: [Effect] }
  deriving Show

data Scope
  = Scope { locals :: Set Text, address :: ScopeAddress }
  deriving Show

empty :: WorldState
empty =
    WorldState {
      variables     = Map.empty
    , closures      = Map.empty
    , scopes        = (NE.singleton $ Scope Set.empty defaultAddr)
    , lastScopeAddr = defaultAddr
    , effects       = []
    }
  where
    defaultAddr = ScopeAddress 0

closeOver :: Text -> WorldState -> WorldState
closeOver varName world = world { closures = newClosures }
  where
    scopeAddr   = world |> scopes &> NE.head &> address
    varAddr     = maybe (error whinerMsg) id $ lookupVarAddr varName world
    newClosures = alter (orEmpty &> (Set.insert scopeAddr) &> Just) varAddr world.closures

    orEmpty setM = maybe Set.empty id setM

    whinerMsg = "Critical error!  It is impossible to close over a variable that has no address: " <> varName

cleanupClosure :: ScopeAddress -> WorldState -> WorldState
cleanupClosure addr world = world { variables = newVars, closures = newClosures }
  where
    csWithoutMe   = map (Set.delete addr) world.closures
    keysOfEmpties = Map.keysSet $ Map.filter (Set.null) csWithoutMe
    newClosures   = Set.foldr Map.delete csWithoutMe     keysOfEmpties
    newVars       = Set.foldr Map.delete world.variables keysOfEmpties

popScope :: WorldState -> WorldState
popScope world = (cleanupClosure myScope.address world) { scopes = newScopes }
  where
    (myScope, tailMNE) = NE.uncons world.scopes
    newScopes          = maybe (error whinerMsg) id tailMNE
    whinerMsg          = "Critical error!  You should never be able to pop the global scope!"

pushScope :: WorldState -> WorldState
pushScope world = world { scopes = newScope <| world.scopes, lastScopeAddr = newAddr }
  where
    newAddr  = ScopeAddress $ world.lastScopeAddr.n + 1
    newScope = Scope Set.empty newAddr

lookupVarAddr :: Text -> WorldState -> Maybe VarAddress
lookupVarAddr varName = scopes &> (foldr concatVarMap Map.empty) &> (lookup varName)
  where
    concatVarMap x acc = x |> locals &> Set.toList &> map (\n -> (n, VarAddress n x.address)) &>
                              Map.fromList &> (flip union acc)

declareVar :: Text -> Value -> WorldState -> WorldState
declareVar varName value world = world { variables = newVars, closures = newClosures, scopes = newScopes }
  where
    (oldScope, tailMNE) = NE.uncons world.scopes
    newScope            = oldScope { locals = Set.insert varName oldScope.locals }
    varAddr             = VarAddress varName newScope.address
    newScopes           = maybe (NE.singleton newScope) (NE.cons newScope) tailMNE
    newVars             = Map.insert varAddr value                            world.variables
    newClosures         = Map.insert varAddr (Set.singleton newScope.address) world.closures

getVar :: Text -> WorldState -> Maybe Value
getVar varName world = world |> (lookupVarAddr varName) &> getValue
  where
    getValue = (>>= (flip lookup world.variables))

setVar :: Text -> Value -> WorldState -> WorldState
setVar varName value world = world { variables = newVars }
  where
    varAddr = maybe (error whinerMsg) id $ lookupVarAddr varName world
    newVars = Map.insert varAddr value world.variables

    whinerMsg = "Critical error!  It is impossible to set a variable that hasn't been declared: " <> varName
