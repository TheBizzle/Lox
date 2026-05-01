module Lox.Evaluator.Internal.OOP(assignIntoObject, classArity, defineClass, indexObject, indexSuper, initObject) where
-- (toSuperChain)

import Control.Monad.State(get, gets, modify)

import Data.List.NonEmpty(NonEmpty((:|)))

import Lox.Scanner.Token(Token(Token), TokenType(Return, This, Var))

import Lox.Parser.AST(
    Expr(Call, VarRef)
  , Statement(ExpressionStatement, FunctionStatement, ReturnStatement)
  , Variable(Variable, varName)
  )

import Lox.Evaluator.Internal.Program(
    Evaluating, Evaluator, Program
  , ProgramState(instanceScopes, lastScopeAddr, nextClosureID, nextInstanceID, scopes, variables)
  )

import Lox.Evaluator.Internal.EvalError(
    EvalError(EvalError)
  , EvalErrorType(ClassNotFound, NotAClass, ObjectLacksKey)
  )

import Lox.Evaluator.Internal.Function(buildFunction, defineFunction, runFunction)
import Lox.Evaluator.Internal.GC(borrowIfFn)

import Lox.Evaluator.Internal.Scope(
    Environment
  , Scope(address, environ)
  , ScopeAddress(n, ScopeAddress)
  , VarAddress(VarAddress)
  )

import Lox.Evaluator.Internal.Value(
    Class(baseEnv, Class, cName, initFnM, methodFns, superclassM)
  , Function(idNum)
  , Object(instanceID, myClass, Object)
  , Value(ClassV, function, FunctionV, Nada, ObjectV)
  )

import Lox.Evaluator.Internal.Variable(currentScope, declareVar, declareVarM, getVar, predictVarAddr, pushScope)

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Maybe         as Maybe

import qualified Lox.Parser.AST as AST

import qualified Lox.Evaluator.Internal.ControlFlow as CF


initName :: Text
initName = "init"

superName :: Text
superName = "super"

thisName :: Text
thisName = "this"

assignIntoObject :: Object -> Text -> Value -> Evaluating
assignIntoObject object propName value =
  do
    addr <- findInObject object propName
    handleAddrM addr
    (mainScope, _, _) <- extractScopeInfo object.myClass.cName object.instanceID
    borrowIfFn mainScope.address value
    return $ Success $ CF.Normal value
  where
    handleAddrM :: Maybe VarAddress -> Program ()
    handleAddrM Nothing     = declareIt object propName value
    handleAddrM (Just addr) = updateIt value addr

    declareIt :: Object -> Text -> Value -> Program ()
    declareIt object propName value =
      do
        let cName                             = object.myClass.cName
        (mainScope, classScopes, instScopes) <- extractScopeInfo cName object.instanceID
        let varAddr                           = VarAddress propName mainScope.address
        let newScope                          = mainScope { environ = Map.insert propName varAddr mainScope.environ }
        let newClassScopes                    = Map.insert cName newScope classScopes
        let newInstScopes                     = Map.insert object.instanceID newClassScopes instScopes
        modify $ \p -> p { instanceScopes = newInstScopes
                         , variables      = Map.insert varAddr value p.variables
                         }

    updateIt :: Value -> VarAddress -> Program ()
    updateIt value addr =
      modify $ \p -> p { variables = Map.insert addr value p.variables }

    extractScopeInfo cName instID =
      do
        instScopes      <- gets instanceScopes
        let classScopes  = Maybe.fromJust $ Map.lookup instID instScopes
        let mainScope    = Maybe.fromJust $ Map.lookup cName  classScopes
        return (mainScope, classScopes, instScopes)

classArity :: Class -> Word
classArity (Class _            _ (Just fn) _ _) = fn |> AST.params &> length &> fromIntegral
classArity (Class _ (Just super)         _ _ _) = classArity super
classArity (Class _            _         _ _ _) = 0

defineClass :: Text -> Maybe Class -> [AST.Function] -> Program Class
defineClass className superClassM functions =
  do
    program       <- get
    let basestEnv  = (currentScope program).environ
    let baseEnv    = Map.insert className (predictVarAddr className program) basestEnv
    let clz        = Class className superClassM (listToMaybe init) methods baseEnv
    modify $ declareVar className $ ClassV clz
    return clz
  where
    (init, methods) = List.partition (AST.fnDecl &> varName &> (== initName)) functions

findInObject :: Object -> Text -> Program (Maybe VarAddress)
findInObject (Object clazz instID) propName =
  do
    envChain   <- toEnvChain clazz instID
    let addrMs  = map (Map.lookup propName) envChain
    return $ join $ find isJust addrMs

-- I find it very objectionable that the 'operator/equals_method' test requires me to make
-- functions not equal to themselves like this.  --Jason B. (3/4/26)
indexObject :: Token -> Object -> Text -> Evaluating
indexObject token object propName =
  do
    addr <- findInObject object propName
    case addr of
      Nothing     -> return $ Failure $ NE.singleton $ EvalError (ObjectLacksKey propName) token
      (Just addr) -> do
        var       <- get <&> (variables &> Map.lookup addr &> Maybe.fromJust)
        properVar <-
          case var of
            (FunctionV isN f _) -> do
              cid <- gets nextClosureID
              modify $ \p -> p { nextClosureID = p.nextClosureID + 1}
              return $ FunctionV isN f $ Just cid
            x -> return x
        return $ Success $ CF.Normal properVar

indexSuper :: Token -> Variable -> Evaluating
indexSuper superToken (Variable propName _) =
  do
    superM <- get <&> (getVar superName)
    maybe (error "`super` used outside of class.  The verifier should have caught this!") helper superM
  where
    helper       Nada = error "`super` used in class without a superclass.  The verifier should have caught this!"
    helper (ClassV s) = (lookupInSupers $ toSuperChain s) <&> (<&> CF.Normal)
    helper          _ = error "Not possible to resolve `super` to something other than a class or nil"

    lookupInSupers supers = maybe (return $ Failure $ NE.singleton $ EvalError (ObjectLacksKey propName) superToken) returnSuperMethod fnM
      where
        fnM =
          if propName /= initName then
            find (AST.fnDecl &> varName &> (== propName)) $ supers >>= methodFns
          else
            (listToMaybe supers) >>= initFnM

    returnSuperMethod (AST.Function name args statements) =
      do
        methodM <- get <&> (getVar trueName)
        fnV     <- maybe (buildSuperMethod <&> (\f -> FunctionV False f Nothing)) return methodM
        return $ Success fnV
      where
        trueName         = "__super_" <> name.varName
        buildSuperMethod = defineFunction trueName (map varName args) statements

initObject :: Token -> Evaluator -> Text -> [Value] -> Evaluating
initObject token evaluator className args =
  do
    program <- get
    case getVar className program of
      Nothing               -> return $ Failure $ NE.singleton $ EvalError (ClassNotFound className) token
      (Just (ClassV clazz)) -> initialize clazz
      (Just              x) -> return $ Failure $ NE.singleton $ EvalError (NotAClass x) token
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

        return $ Success $ CF.Normal objV

    fillInClass :: VarAddress -> Word -> Class -> Program ()
    fillInClass thisAddr instID clazz =
      do
        modify $ pushScope $ Map.insert thisName thisAddr clazz.baseEnv

        declareVarM superName $ maybe Nada ClassV clazz.superclassM

        method2s <- mapM                 buildFn                clazz.methodFns
        init2M   <- mapM (convertInit &> buildFn) $ maybeToList clazz.initFnM

        modify $ \p -> p { scopes = (((NE.head p.scopes) { environ = Map.empty }) :| (NE.tail p.scopes)) }
        forM_ (method2s <> init2M) (\(name, fn) -> declareVarM name $ FunctionV False fn Nothing)

        modify $ popInstanceScope clazz.cName instID
      where
        -- Yikes.  So I guess he wants us to be able to call the constructor on an instance as a method, and
        -- to get back the same object that it was called on.  Dislike. --Jason B. (3/9/26)
        convertInit (AST.Function decl args body) =
          (AST.Function decl args $
            [ (FunctionStatement (AST.Function (Variable "__init" (Token Var (err 0))) [] body))
            , (ExpressionStatement (Call (VarRef (Variable "__init" (Token Var (err 1)))) (Token Var (err 2)) []))
            , (ReturnStatement (Token Return (err 3)) (Just (AST.This (Token This (err 4)))))
            ]
          )

        err :: Int -> a
        err n = error $ "I did a bad thing" <> (showText n)

    buildFn :: AST.Function -> Program (Text, Function)
    buildFn (AST.Function decl args body) = (buildFunction decl.varName (map varName args) body) <&> (decl.varName, )

toSuperChain :: Class -> [Class]
toSuperChain s = s : (maybe [] toSuperChain $ s.superclassM)

toEnvChain :: Class -> Word -> Program [Environment]
toEnvChain clazz instID =
  do
    instScopes   <- gets instanceScopes
    let instEnvs  = Maybe.fromJust $ Map.lookup instID instScopes
    let envs      = Maybe.fromJust $ mapM (cName &> flip Map.lookup instEnvs &> (map environ)) $ toSuperChain clazz
    return envs

popInstanceScope :: Text -> Word -> ProgramState -> ProgramState
popInstanceScope className instanceID program =
    program { scopes         = newScopes
            , instanceScopes = newInstScopes
            }
  where
    (myScope, tailMNE) = NE.uncons program.scopes
    newScopes          = tailMNE `orElse` (error whinerMsg)
    whinerMsg          = "Critical error!  The top scope cannot possibly be a class scope, nor is it even poppable!"
    newInstScopes      = Map.alter (flip orElse Map.empty &> Map.insert className myScope &> Just) instanceID program.instanceScopes
