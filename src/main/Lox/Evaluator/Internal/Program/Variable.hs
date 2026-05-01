module Lox.Evaluator.Internal.Program.Variable(
    currentEnvironment, currentScope, declareVar, declareVarM, getVar, popScope, predictVarAddr, pushScope, setVar
  ) where

import Control.Monad.State(get, gets, modify)

import Data.List.NonEmpty((<|))
import Data.Map(lookup)

import Lox.Evaluator.Internal.Program.GC(borrowIfFn, cleanupScope)

import Lox.Evaluator.Internal.Program.Program(
    Program
  , ProgramState(lastScopeAddr, scopes, variables)
  )

import Lox.Evaluator.Internal.Program.Scope(
    Environment, Scope(address, environ, Scope), ScopeAddress(n, ScopeAddress)
  , VarAddress(VarAddress, scopeAddr)
  )

import Lox.Evaluator.Internal.Program.Value(Value)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map


currentEnvironment :: Program Environment
currentEnvironment = get <&> (currentScope &> environ)

currentScope :: ProgramState -> Scope
currentScope = scopes &> NE.head

popScope :: ProgramState -> ProgramState
popScope program = (cleanupScope myScope program) { scopes = newScopes }
  where
    (myScope, tailMNE) = NE.uncons program.scopes
    newScopes          = tailMNE `orElse` (error whinerMsg)
    whinerMsg          = "Critical error!  You should never be able to pop the global scope!"

pushScope :: Environment -> ProgramState -> ProgramState
pushScope env program = program { scopes = newScope <| program.scopes, lastScopeAddr = newAddr }
  where
    newAddr  = ScopeAddress $ program.lastScopeAddr.n + 1
    newScope = Scope env newAddr

lookupVarAddr :: Text -> ProgramState -> Maybe VarAddress
lookupVarAddr varName = currentScope &> environ &> (lookup varName)

declareVarM :: Text -> Value -> Program ()
declareVarM varName value = modify $ declareVar varName value

declareVar :: Text -> Value -> ProgramState -> ProgramState
declareVar varName value program = program { variables = newVars, scopes = newScopes }
  where
    (oldScope, tailML) = NE.uncons program.scopes
    varAddr            = predictVarAddr varName program
    newScope           = oldScope { environ = Map.insert varName varAddr oldScope.environ }
    newScopes          = maybe (NE.singleton newScope) (NE.cons newScope) tailML
    newVars            = Map.alter (const $ Just value) varAddr program.variables

getVar :: Text -> ProgramState -> Maybe Value
getVar varName program = program |> (lookupVarAddr varName) &> getValue
  where
    getValue = (>>= (flip lookup program.variables))

predictVarAddr :: Text -> ProgramState -> VarAddress
predictVarAddr varName program = VarAddress varName (currentScope program).address

setVar :: Text -> Value -> Program ()
setVar varName value =
  do
    varAddr <- gets $ (lookupVarAddr varName) &> (`orElse` (error whinerMsg))
    borrowIfFn varAddr.scopeAddr value
    modify $ \p -> p { variables = Map.insert varAddr value p.variables }
  where
    whinerMsg = "Critical error!  It is impossible to set a variable that hasn't been declared: " <> varName
