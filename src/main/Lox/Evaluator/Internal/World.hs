module Lox.Evaluator.Internal.World(
    closeOver, declareVar, empty, getVar, popScope, pushScope, runEffect, setVar
  , World
  , WorldState(scopes, WorldState)
  ) where

import Control.Monad.State(get, modify, StateT)

import Data.List.NonEmpty((<|))
import Data.Map(alter, lookup)

import Lox.Evaluator.Internal.Data(ScopeAddress(n, ScopeAddress), VarAddress(scopeAddr, VarAddress))
import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Value(Value(NilV))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text.IO       as TIO

import qualified Lox.Evaluator.Internal.ControlFlow as CF


type World   t  = StateT WorldState IO t
type Result  t  = Validation (NonEmpty EvalError) t
type Worldly t  = World (Result t)
type Evaluated  = Worldly Value
type Evaluating = Worldly CF.ControlFlow



data WorldState
  = WorldState { variables        :: Map VarAddress Value              -- The central registry of all variables' values
               , closures         :: Map VarAddress (Set ScopeAddress) -- Keep-alives for all closed-over variables
               , scopes           :: NonEmpty Scope                    -- The call stack
               , lastScopeAddr    :: ScopeAddress
               }
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

runEffect :: Effect -> Evaluated
runEffect (Print x) = (liftIO $ TIO.putStrLn x) $> (Success NilV)

    whinerMsg = "Critical error!  It is impossible to close over a variable that has no address: " <> varName

cleanupClosure :: ScopeAddress -> WorldState -> WorldState
cleanupClosure addr world = world { variables = newVars, closures = newClosures }
  where
    csWithoutMe   = map (Set.delete addr) world.closures
    keysOfEmpties = Map.keysSet $ Map.filter (Set.null) csWithoutMe
    newClosures   = Set.foldr Map.delete csWithoutMe     keysOfEmpties
    newVars       = Set.foldr Map.delete world.variables keysOfEmpties

currentScope :: WorldState -> Scope
currentScope = scopes &> NE.head

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
