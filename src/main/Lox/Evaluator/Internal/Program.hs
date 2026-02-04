module Lox.Evaluator.Internal.Program(
    arity, assignIntoObject, currentEnvironment, declareVar, defineClass, defineFunction, definePrimitiveFunc, empty, failOrM, getVar, indexObject, indexSuper, initObject, popScope, pushScope, runEffect, runFunction, setVar, transferOwnership
  , Program
  , ProgramState(scopes, ProgramState)
  ) where

import Control.DeepSeq(NFData, rnf)
import Control.Monad.State(get, gets, modify, StateT)

import Data.List.NonEmpty((<|), NonEmpty((:|)))
import Data.Map(alter, lookup)
import Data.Maybe(fromJust)

import Lox.Scanner.Token(TokenPlus)

import Lox.Parser.AST(Statement)

import Lox.Evaluator.Internal.Data(
    Environment
  , Scope(address, environ, Scope)
  , ScopeAddress(n, ScopeAddress)
  , VarAddress(scopeAddr, VarAddress)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.EvalError(EvalError(CanOnlyRefSuperInsideClass, ClassNotFound, NotAClass, ObjectLacksKey, ThisClassHasNoSupers))

import Lox.Evaluator.Internal.Value(
    Class(baseEnv, Class, cName, initOutlineM, methodOutlines, superclassM)
  , Function(argNames, env, Function, idNum, name)
  , Object(instanceID, myClass, Object)
  , Value(ClassV, function, FunctionV, Nada, NilV, ObjectV)
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Maybe         as Maybe
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

type ClassScopes = Map Text Scope

instance Show Func where
  show _ = "<underlying_function>"

arity :: Value -> Maybe Word
arity (FunctionV _ fn) = fn |> argNames &> length &> fromIntegral &> Just
arity (ClassV       c) = c  |> initOutlineM &> maybe 0 (snd3 &> length &> fromIntegral) |> Just
arity _                = Nothing

data ProgramState
  = ProgramState { variables        :: Map VarAddress Value      -- The central registry of all variables' values
                 , closures         :: Map VarAddress (Set FnID) -- Keep-alives for all closed-over variables
                 , functions        :: Map FnID Func             -- Definitions of all living functions
                 , transferredFuncs :: Map FnID VarAddress       -- A function returned as a callback, whose owning scope is now dead
                 , nextFnNum        :: FnID
                 , scopes           :: NonEmpty Scope            -- The call stack
                 , lastScopeAddr    :: ScopeAddress
                 , instanceScopes   :: Map Word ClassScopes      -- Maps ID nums to maps from class name to scope
                 , nextInstanceID   :: Word
                 }
  deriving Show

instance NFData ProgramState where
    rnf (ProgramState vs cs fns xfs nfn ss lsa cse ncsn) =
      rnf vs `seq` rnf cs `seq` rnf fns `seq` rnf xfs `seq` rnf nfn `seq` rnf ss `seq` rnf lsa `seq` rnf cse `seq` rnf ncsn

initName :: Text
initName = "init"

superName :: Text
superName = "super"

thisName :: Text
thisName = "this"

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
    , instanceScopes   = Map.empty
    , nextInstanceID   = 0
    }
  where
    defaultAddr = ScopeAddress 0

assignIntoObject :: Object -> Text -> Value -> Evaluating
assignIntoObject object propName value =
  do
    addr <- findInObject object propName
    handleAddrM addr
    return $ Success $ CF.Normal value
  where
    handleAddrM :: Maybe VarAddress -> Program ()
    handleAddrM Nothing     = declareIt object propName value
    handleAddrM (Just addr) = updateIt value addr

    declareIt :: Object -> Text -> Value -> Program ()
    declareIt object propName value =
      do
        (mainScope, classScopes, instScopes, cName) <- extractScopeInfo object.myClass.cName object.instanceID
        let varAddr                                  = VarAddress propName mainScope.address
        let newScope                                 = mainScope { environ = Map.insert propName varAddr mainScope.environ }
        let newClassScopes                           = Map.insert cName newScope classScopes
        let newInstScopes                            = Map.insert object.instanceID newClassScopes instScopes
        modify $ \p -> p { instanceScopes = newInstScopes
                         , variables      = Map.insert varAddr value p.variables
                         }

    updateIt :: Value -> VarAddress -> Program ()
    updateIt value addr =
      modify $ \p -> p { variables = Map.insert addr value p.variables }

    extractScopeInfo cName instID =
      do
        instScopes      <- gets instanceScopes
        let classScopes  = fromJust $ Map.lookup instID instScopes
        let mainScope    = fromJust $ Map.lookup cName  classScopes
        return (mainScope, classScopes, instScopes, cName)

cleanupScope :: Scope -> ProgramState -> ProgramState
cleanupScope scope program = program { variables = cleanedVars, closures = cleanedClosures, functions = cleanedFunctions, transferredFuncs = cleanedXFns }
  where
    locals :: Environment
    locals          = Map.filter (scopeAddr &> (== scope.address)) scope.environ
    (fAs, xAs)      = Map.partition checkIsFunction     locals
    (tAs, dyingFAs) = Map.partition checkWasTransferred fAs

    -- Clean up all of my local vars that haven't been closed over
    closedOverAs = (Map.elems tAs) >>= (\tA -> Map.lookup tA program.variables |> maybe (error "Can't be dead") (function &> env) &> Map.elems)
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
        isFunction (FunctionV _ _) = True
        isFunction               _ = False
        noBueno                    = "You can't possibly be cleaning up a variable that's already been cleaned up...."

    checkWasTransferred = checkVar wasTransferred noBueno
      where
        wasTransferred (FunctionV _ fn) = Map.member fn.idNum program.transferredFuncs
        wasTransferred                _ = error "Not possible!  We already proved these are only functions!"
        noBueno                         = "You can't possibly be checking a function that's already been cleaned up...."

    toFnID = checkVar (function &> idNum) "You can't possibly be getting the FnID of a function that's already been cleaned up...."

    checkVar :: (Value -> a) -> Text -> VarAddress -> a
    checkVar conversion errorMsg = (flip Map.lookup program.variables) &> maybe (error errorMsg) conversion

defineClass :: Text -> Maybe Class -> [(Text, [Text], [Statement])] -> Program Class
defineClass className superClassM methodTriples =
  do
    program       <- get
    let basestEnv  = (currentScope program).environ
    let baseEnv    = Map.insert className (predictVarAddr className program) basestEnv
    let clz        = Class className superClassM (listToMaybe init) triples baseEnv
    modify $ declareVar className $ ClassV clz
    return clz
  where
    (init, triples) = List.partition (fst3 &> (== initName)) methodTriples

defineFunction :: Text -> [Text] -> [Statement] -> Program Function
defineFunction name argNames body =
  do
    fn <- buildFunction name argNames body
    modify $ declareVar name $ FunctionV False fn
    return fn

buildFunction :: Text -> [Text] -> [Statement] -> Program Function
buildFunction name argNames body =
  do
    envir <- gets $ currentScope &> environ
    _defineFunction name argNames $ inner envir
  where
    inner :: Environment -> Func
    inner envir eval args =
      do
        modify $ pushScope envir
        let bindings = argNames `List.zip` args
        modify $ \w -> foldr (uncurry declareVar) w bindings
        resultV <- foldM (whileNormal eval) (Success $ CF.Normal Nada) body
        modify popScope
        return $ second convert resultV

    whileNormal f (Success (CF.Normal _)) s = f s
    whileNormal _ (Success             x) _ = return $ Success x
    whileNormal _                       x _ = return x

    convert (CF.Normal _) = CF.Normal Nada
    convert (CF.Return x) = CF.Normal x
    convert             x = x

definePrimitiveFunc :: Text -> [Text] -> Func -> Program ()
definePrimitiveFunc name args func =
  do
    fn <- _defineFunction name args func
    modify $ declareVar name $ FunctionV True fn

_defineFunction :: Text -> [Text] -> Func -> Program Function
_defineFunction name argNames func =
  do
    program          <- get
    let scope         = currentScope program
    let fnNum         = program.nextFnNum
    let upsert        = alter (orEmpty &> (Set.insert fnNum) &> Just)
    let newClosures   = Map.foldr upsert program.closures scope.environ
    let fn            = Function name argNames scope.environ fnNum scope.address
    let newFunctions  = Map.insert fnNum func program.functions
    modify $ \p -> p { closures = newClosures, functions = newFunctions, nextFnNum = fnNum + 1 }
    return fn
  where
    orEmpty setM = maybe Set.empty id setM
    -- Possible optimization: Read through the statements and figure out which variables truly need to be closed over.
    -- Jason B. (12/25/25)

findInObject :: Object -> Text -> Program (Maybe VarAddress)
findInObject (Object clazz instID) propName =
  do
    envChain   <- toEnvChain clazz instID
    let addrMs  = map (Map.lookup propName) envChain
    return $ join $ find isJust addrMs

indexObject :: Object -> Text -> Evaluating
indexObject object propName =
  do
    addr <- findInObject object propName
    case addr of
      Nothing     -> return $ Failure $ NE.singleton $ ObjectLacksKey propName
      (Just addr) -> get <&> (variables &> Map.lookup addr &> Maybe.fromJust &> CF.Normal &> Success)

indexSuper :: TokenPlus -> Text -> Evaluating
indexSuper superTP propName =
  do
    superM <- get <&> (getVar superName)
    maybe (return $ Failure $ NE.singleton $ CanOnlyRefSuperInsideClass superTP) indexSuper superM
  where
    indexSuper NilV       = return $ Failure $ NE.singleton $ ThisClassHasNoSupers superTP
    indexSuper (ClassV s) = (lookupInSupers $ toSuperChain s) <&> (<&> CF.Normal)
    indexSuper          _ = error "Not possible to resolve `super` to something other than a class or nil"

    lookupInSupers supers = maybe (return $ Failure $ NE.singleton $ ObjectLacksKey propName) returnSuperMethod tripleM
      where
        tripleM = find (fst3 &> (== propName)) $ supers >>= methodOutlines

    returnSuperMethod (name, args, statements) =
      do
        methodM <- get <&> (getVar trueName)
        fnV     <- maybe (buildSuperMethod <&> (FunctionV False)) return methodM
        return $ Success fnV
      where
        trueName         = "__super_" <> name
        buildSuperMethod = defineFunction trueName args statements

initObject :: Evaluator -> Text -> [Value] -> Evaluating
initObject evaluator className args =
  do
    program <- get
    case getVar className program of
      Nothing               -> return $ Failure $ NE.singleton $ ClassNotFound className
      (Just (ClassV clazz)) -> initialize clazz
      (Just              x) -> return $ Failure $ NE.singleton $ NotAClass x
  where
    initialize clazz =
      do
        thisAddr <- gets $ \p -> VarAddress thisName $ ScopeAddress $ p.lastScopeAddr.n + 1
        modify $ \p -> p { variables = Map.insert thisAddr Nada p.variables } -- Temp value; see below

        instID      <- gets nextInstanceID
        let classes  = toSuperChain clazz
        let obj      = Object clazz instID
        let objV     = ObjectV obj
        modify $ \p -> p { variables      = Map.insert thisAddr objV p.variables
                         , nextInstanceID = instID + 1
                         }
        forM_ classes $ fillInClass thisAddr instID

        initializerAddrM <- findInObject obj initName
        initializerM     <- gets $ \p -> initializerAddrM >>= (flip Map.lookup p.variables)
        forM_ initializerM $ \initializer -> runFunction evaluator initializer.function.idNum args

        void $ removeInits (map cName classes) instID

        return $ Success $ CF.Normal objV

    fillInClass :: VarAddress -> Word -> Class -> Program ()
    fillInClass thisAddr instID clazz =
      do
        modify $ pushScope $ Map.insert thisName thisAddr clazz.baseEnv

        declareVarM superName $ maybe Nada ClassV clazz.superclassM

        method2s <- mapM (\(n, as, b) -> (buildFunction n as b) <&> (n, ))               clazz.methodOutlines
        init2M   <- mapM (\(n, as, b) -> (buildFunction n as b) <&> (n, )) $ maybeToList clazz.initOutlineM

        modify $ \p -> p { scopes = (((NE.head p.scopes) { environ = Map.empty }) :| (NE.tail p.scopes)) }
        forM_ (method2s <> init2M) (\(name, fn) -> declareVarM name $ FunctionV False fn)

        modify $ popInstanceScope clazz.cName instID

    removeInits classNames instID =
      do
        classScopes <- gets $ instanceScopes &> Map.lookup instID &> fromJust
        forM classNames $ \className -> do
          let (Scope env saddr) = fromJust $ Map.lookup className classScopes
          case Map.lookup initName env of
            Nothing   -> return ()
            (Just fa) -> do
              let newEnv      = Map.delete initName env
              let newScope    = Scope newEnv saddr
              let newCScopes  = Map.insert className newScope classScopes
              fid            <- gets $ variables &> Map.lookup fa &> Maybe.fromJust &> getFID
              modify $ \p -> p { instanceScopes = Map.insert instID newCScopes p.instanceScopes
                               , functions      = Map.delete fid p.functions
                               , variables      = Map.delete fa  p.variables
                               }
      where
        getFID (FunctionV _ fn) = fn.idNum
        getFID                _ = error "Function is not a function!"

runFunction :: Evaluator -> FnID -> [Value] -> Evaluating
runFunction evaluator idNum args =
  do
    underlyingM    <- gets $ \s -> Map.lookup idNum s.functions
    let underlying  = maybe (error whinerMsg) id underlyingM
    underlying evaluator args
  where
    whinerMsg = "Critical error!  You should never be able to run a non-existent function: " <> (showText idNum)

runEffect :: Effect -> Evaluated
runEffect (Print x) = (liftIO $ TIO.putStrLn x) $> (Success Nada)

transferOwnership :: Value -> Program ()
transferOwnership (FunctionV _ fn) = _transferOwnership fn.name fn.idNum
transferOwnership                _ = return ()

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

popInstanceScope :: Text -> Word -> ProgramState -> ProgramState
popInstanceScope className instanceID program =
    program { scopes         = newScopes
            , instanceScopes = newInstScopes
            }
  where
    (myScope, tailMNE) = NE.uncons program.scopes
    newScopes          = maybe (error whinerMsg) id tailMNE
    whinerMsg          = "Critical error!  The top scope cannot possibly be a class scope, nor is it even poppable!"
    newInstScopes      = Map.alter (maybe Map.empty id &> Map.insert className myScope &> Just) instanceID program.instanceScopes

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

predictVarAddr :: Text -> ProgramState -> VarAddress
predictVarAddr varName program = VarAddress varName (currentScope program).address

setVar :: Text -> Value -> ProgramState -> ProgramState
setVar varName value program = program { variables = newVars }
  where
    varAddr = maybe (error whinerMsg) id $ lookupVarAddr varName program
    newVars = Map.insert varAddr value program.variables

    whinerMsg = "Critical error!  It is impossible to set a variable that hasn't been declared: " <> varName

pruneSet :: Ord a => Set a -> Set a -> Maybe (Set a)
pruneSet baddies current =
  if not $ Set.null res then
    Just res
  else
    Nothing
  where
    res = Set.filter (`Set.member` baddies) current

toEnvChain :: Class -> Word -> Program [Environment]
toEnvChain clazz instID =
  do
    instScopes   <- gets instanceScopes
    let instEnvs  = fromJust $ Map.lookup instID instScopes
    let envs      = fromJust $ mapM (cName &> flip Map.lookup instEnvs &> (map environ)) $ toSuperChain clazz
    return envs

toSuperChain :: Class -> [Class]
toSuperChain s = s : (maybe [] toSuperChain $ s.superclassM)

failOrM :: Monad m => Validation f s1 -> (s1 -> m (Validation f s2)) -> m (Validation f s2)
v `failOrM` vm = validation (Failure &> return) vm v
