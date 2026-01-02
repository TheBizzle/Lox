module Lox.Evaluator.Internal.Program(
    arity, currentEnvironment, declareVar, defineFunction, definePrimitiveFunc, empty, getVar, popScope, pushScope, runEffect, runFunction, setVar, transferOwnership
  , Program
  , ProgramState(scopes, ProgramState)
  ) where

import Control.DeepSeq(NFData, rnf)
import Control.Monad.State(get, modify, StateT)

import Data.List.NonEmpty((<|))
import Data.Map(alter, lookup)

import Lox.Parser.AST(Statement)

import Lox.Evaluator.Internal.Data(Environment, ScopeAddress(n, ScopeAddress), VarAddress(scopeAddr, VarAddress))
import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.EvalError(EvalError)
import Lox.Evaluator.Internal.Value(Value(argNames, env, FunctionV, idNum, NilV))

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text.IO       as TIO

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

arity :: Value -> Maybe Word
arity f@(FunctionV _ _ _ _ _) = f |> argNames &> length &> fromIntegral &> Just
arity _                       = Nothing

data ProgramState
  = ProgramState { variables        :: Map VarAddress Value      -- The central registry of all variables' values
                 , closures         :: Map VarAddress (Set FnID) -- Keep-alives for all closed-over variables
                 , functions        :: Map FnID Func             -- Definitions of all living functions
                 , transferredFuncs :: Map FnID VarAddress       -- A function returned as a callback, whose owning scope is now dead
                 , nextFnNum        :: FnID
                 , scopes           :: NonEmpty Scope            -- The call stack
                 , lastScopeAddr    :: ScopeAddress
                 }
  deriving Show

instance NFData ProgramState where
    rnf (ProgramState vs cs fns xfs nfn ss lsa) =
      rnf vs `seq` rnf cs `seq` rnf fns `seq` rnf xfs `seq` rnf nfn `seq` rnf ss `seq` rnf lsa

data Scope
  = Scope { environ :: Environment, address :: ScopeAddress }
  deriving Show

instance NFData Scope where
    rnf (Scope env addr) = rnf env `seq` rnf addr

empty :: ProgramState
empty =
    ProgramState {
      variables        = Map.empty
    , closures         = Map.empty
    , functions        = Map.empty
    , transferredFuncs = Map.empty
    , nextFnNum        = 0
    , scopes           = (NE.singleton $ Scope Map.empty defaultAddr)
    , lastScopeAddr    = defaultAddr
    }
  where
    defaultAddr = ScopeAddress 0

cleanupScope :: Scope -> ProgramState -> ProgramState
cleanupScope scope program = program { variables = cleanedVars, closures = cleanedClosures, functions = cleanedFunctions, transferredFuncs = cleanedXFns }
  where
    locals :: Environment
    locals          = Map.filter (scopeAddr &> (== scope.address)) scope.environ
    (fAs, xAs)      = Map.partition checkIsFunction     locals
    (tAs, dyingFAs) = Map.partition checkWasTransferred fAs

    -- Clean up all of my local vars that haven't been closed over
    closedOverAs = (Map.elems tAs) >>= (\tA -> Map.lookup tA program.variables |> maybe (error "Can't be dead") env &> Map.elems)
    dyingVAs     = (Set.fromList $ Map.elems xAs) `Set.difference` (Set.fromList closedOverAs)
    cleanedVars  = Set.foldr Map.delete program.variables dyingVAs

    -- If it was transferred to me, remove it from the list of transferreds
    xedToMeFns  = Map.filter (scopeAddr &> (== scope.address)) tAs
    cleanedXFns = foldr (toFnID &> Map.delete) program.transferredFuncs $ Map.elems xedToMeFns

    -- If it was transferred to me and isn't still available at its defining address, delete it entirely
    -- Also, remove all of my local functions' closures from the list of transferreds
    dyingXFns        = Map.filterWithKey (\_ addr -> not $ Map.member addr cleanedVars) xedToMeFns
    dyingXFnIDs      = Set.fromList $ map toFnID $ Map.elems dyingXFns
    dyingLocalFnIDs  = Set.fromList $ map toFnID $ Map.elems dyingFAs
    dyingFnIDs       = dyingXFnIDs `Set.union` dyingLocalFnIDs
    cleanedClosures  = Map.mapMaybe (pruneSet dyingFnIDs) program.closures
    cleanedFunctions = Set.foldr Map.delete program.functions dyingFnIDs

    checkIsFunction = checkVar isFunction noBueno
      where
        isFunction (FunctionV _ _ _ _ _) = True
        isFunction                     _ = False
        noBueno                          = "You can't possibly be cleaning up a variable that's already been cleaned up...."

    checkWasTransferred = checkVar wasTransferred noBueno
      where
        wasTransferred (FunctionV _ _ _ idNum _) = Map.member idNum program.transferredFuncs
        wasTransferred                         _ = error "Not possible!  We already proved these are only functions!"
        noBueno                                  = "You can't possibly be checking a function that's already been cleaned up...."

    toFnID = checkVar idNum "You can't possibly be getting the FnID of a function that's already been cleaned up...."

    checkVar :: (Value -> a) -> Text -> VarAddress -> a
    checkVar conversion errorMsg = (flip Map.lookup program.variables) &> maybe (error errorMsg) conversion

    pruneSet baddies current =
      if not $ Set.null res then
        Just res
      else
        Nothing
      where
        res = Set.filter (`Set.member` baddies) current

defineFunction :: Text -> [Text] -> [Statement] -> ProgramState -> ProgramState
defineFunction name argNames body program = _defineFunction name argNames inner program
  where
    inner :: Func
    inner eval args =
      do
        modify $ pushScope envir
        let bindings = argNames `List.zip` args
        modify $ \w -> foldr (uncurry declareVar) w bindings
        resultV <- foldM (whileNormal eval) (Success $ CF.Normal NilV) body
        modify popScope
        return $ second convert resultV

    envir = (currentScope program).environ

    whileNormal f (Success (CF.Normal _)) s = f s
    whileNormal _ (Success             x) _ = return $ Success x
    whileNormal _                       x _ = return x

    convert (CF.Normal _) = CF.Normal NilV
    convert (CF.Return x) = CF.Normal x
    convert             x = x

definePrimitiveFunc :: Text -> [Text] -> Func -> ProgramState -> ProgramState
definePrimitiveFunc = _defineFunction

_defineFunction :: Text -> [Text] -> Func -> ProgramState -> ProgramState
_defineFunction name argNames func program = declareVar name fn funcyProgram
  where
    fnNum = program.nextFnNum

    scope        = currentScope program
    newClosures  = Map.foldr upsert program.closures scope.environ
    upsert       = alter (orEmpty &> (Set.insert fnNum) &> Just)
    orEmpty setM = maybe Set.empty id setM
    -- Possible optimization: Read through the statements and figure out which variables truly need to be closed over.
    -- Jason B. (12/25/25)

    fn           = FunctionV name argNames scope.environ fnNum scope.address
    newFunctions = Map.insert fnNum func program.functions
    funcyProgram = program { closures = newClosures, functions = newFunctions, nextFnNum = fnNum + 1 }

runFunction :: Evaluator -> [Value] -> FnID -> Evaluating
runFunction evaluator args idNum =
  do
    state           <- get
    let underlyingM  = Map.lookup idNum state.functions
    let underlying   = maybe (error whinerMsg) id underlyingM
    underlying evaluator args
  where
    whinerMsg = "Critical error!  You should never be able to run a non-existent function: " <> (showText idNum)

runEffect :: Effect -> Evaluated
runEffect (Print x) = (liftIO $ TIO.putStrLn x) $> (Success NilV)

transferOwnership :: Value -> Program ()
transferOwnership (FunctionV name _ _ idNum _) = _transferOwnership name idNum
transferOwnership                            _ = return ()

_transferOwnership :: Text -> FnID -> Program ()
_transferOwnership fnName idNum = modify $ \program -> program { transferredFuncs = newXferred program }
  where
    newXferred program = Map.insert idNum (VarAddress fnName $ parentAddr program) program.transferredFuncs
    parentAddr program = (neSecond program.scopes).address
    neSecond      ne = case NE.tail ne of
                         (h:_) -> h
                         []    -> error "Not possible!  You can't call `return` from the top scope!"

currentEnvironment :: Program Environment
currentEnvironment = get <&> (currentScope &> environ)

currentScope :: ProgramState -> Scope
currentScope = scopes &> NE.head

popScope :: ProgramState -> ProgramState
popScope program = (cleanupScope myScope program) { scopes = newScopes }
  where
    (myScope, tailMNE) = NE.uncons program.scopes
    newScopes          = maybe (error whinerMsg) id tailMNE
    whinerMsg          = "Critical error!  You should never be able to pop the global scope!"

pushScope :: Environment -> ProgramState -> ProgramState
pushScope env program = program { scopes = newScope <| program.scopes, lastScopeAddr = newAddr }
  where
    newAddr  = ScopeAddress $ program.lastScopeAddr.n + 1
    newScope = Scope env newAddr

lookupVarAddr :: Text -> ProgramState -> Maybe VarAddress
lookupVarAddr varName = currentScope &> environ &> (lookup varName)

declareVar :: Text -> Value -> ProgramState -> ProgramState
declareVar varName value program = program { variables = newVars, scopes = newScopes }
  where
    (oldScope, tailML) = NE.uncons program.scopes
    varAddr            = VarAddress varName oldScope.address
    newScope           = oldScope { environ = Map.insert varName varAddr oldScope.environ }
    newScopes          = maybe (NE.singleton newScope) (NE.cons newScope) tailML
    newVars            = Map.alter (updater value) varAddr program.variables
    updater value old  =
      if (isNothing old) || (isNothing tailML) then
        Just value
      else
        error $ "There is already a variable named '" <> varName <> "' in this scope."

getVar :: Text -> ProgramState -> Maybe Value
getVar varName program = program |> (lookupVarAddr varName) &> getValue
  where
    getValue = (>>= (flip lookup program.variables))

setVar :: Text -> Value -> ProgramState -> ProgramState
setVar varName value program = program { variables = newVars }
  where
    varAddr = maybe (error whinerMsg) id $ lookupVarAddr varName program
    newVars = Map.insert varAddr value program.variables

    whinerMsg = "Critical error!  It is impossible to set a variable that hasn't been declared: " <> varName
