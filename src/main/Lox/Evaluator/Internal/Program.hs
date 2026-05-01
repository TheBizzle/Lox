module Lox.Evaluator.Internal.Program(
    empty, Evaluated, Evaluating, Evaluator, FnID, Func, Prog, Program
  , ProgramState(closures, fnBorrowers, functions, instanceScopes, lastScopeAddr, nextClosureID, nextFnNum, nextInstanceID, ProgramState, scopes, variables)
  ) where

import Control.DeepSeq(NFData, rnf)
import Control.Monad.State(StateT)

import Lox.Parser.AST(Statement)

import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Scope(Scope(Scope), ScopeAddress(ScopeAddress), VarAddress)
import Lox.Evaluator.Internal.Value(Value)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map

import qualified Lox.Evaluator.Internal.ControlFlow as CF


type FnID       = Word
type Program t  = StateT ProgramState IO t
type Result  t  = Validation (NonEmpty EvalError) t
type Prog    t  = Program (Result t)
type Evaluated  = Prog Value
type Evaluating = Prog CF.ControlFlow
type Evaluator  = Statement -> Evaluating
type Func       = Evaluator -> [Value] -> Evaluating

instance Show Func where
  show _ = "<underlying_function>"

type ClassScopes = Map Text Scope

empty :: ProgramState
empty =
    ProgramState {
      variables      = Map.empty
    , closures       = Map.empty
    , functions      = Map.empty
    , fnBorrowers    = Map.empty
    , nextFnNum      = 0
    , scopes         = (NE.singleton $ Scope Map.empty defaultAddr)
    , lastScopeAddr  = defaultAddr
    , instanceScopes = Map.empty
    , nextInstanceID = 0
    , nextClosureID  = 0
    }
  where
    defaultAddr = ScopeAddress 0

data ProgramState
  = ProgramState { variables      :: Map VarAddress Value      -- The central registry of all variables' values
                 , closures       :: Map VarAddress (Set FnID) -- Keep-alives for all closed-over variables
                 , functions      :: Map FnID Func             -- Definitions of all living functions
                 , fnBorrowers    :: Map FnID (Set VarAddress) -- Borrowers of a function whose original owning scope is now dead
                 , nextFnNum      :: FnID
                 , scopes         :: NonEmpty Scope            -- The call stack
                 , lastScopeAddr  :: ScopeAddress
                 , instanceScopes :: Map Word ClassScopes      -- Maps ID nums to maps from class name to scope
                 , nextInstanceID :: Word
                 , nextClosureID  :: Word
                 }
  deriving Show

instance NFData ProgramState where
    rnf (ProgramState vs cs fns xfs nfn ss lsa cse ncsn ncid) =
      rnf vs `seq` rnf cs `seq` rnf fns `seq` rnf xfs `seq` rnf nfn `seq` rnf ss `seq` rnf lsa `seq`
        rnf cse `seq` rnf ncsn `seq` rnf ncid
