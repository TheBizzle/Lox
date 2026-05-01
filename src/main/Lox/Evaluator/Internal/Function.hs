module Lox.Evaluator.Internal.Function(buildFunction, defineFunction, definePrimitiveFunc, runFunction) where

import Control.Monad.State(get, gets, modify)

import Lox.Parser.AST(Statement)

import Lox.Evaluator.Internal.Program(
    Evaluating, Evaluator, FnID, Func, Program
  , ProgramState(closures, functions, nextFnNum)
  )

import Lox.Evaluator.Internal.Scope(Environment, Scope(address, environ))
import Lox.Evaluator.Internal.Value(Function(Function), Value(FunctionV, Nada))
import Lox.Evaluator.Internal.Variable(currentScope, declareVar, popScope, pushScope, setVar)

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import qualified Lox.Evaluator.Internal.ControlFlow as CF


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

    convert (CF.Normal   _) = CF.Normal Nada
    convert (CF.Return _ x) = CF.Normal x
    convert               x = x

defineFunction :: Text -> [Text] -> [Statement] -> Program Function
defineFunction name argNames body =
  do
    modify $ declareVar name Nada
    fn <- buildFunction name argNames body
    setVar name $ FunctionV False fn Nothing
    return fn

definePrimitiveFunc :: Text -> [Text] -> Func -> Program ()
definePrimitiveFunc name args func =
  do
    fn <- _defineFunction name args func
    modify $ declareVar name $ FunctionV True fn Nothing

_defineFunction :: Text -> [Text] -> Func -> Program Function
_defineFunction name argNames func =
  do
    program          <- get
    let scope         = currentScope program
    let fnNum         = program.nextFnNum
    let upsert        = Map.alter (orEmpty &> (Set.insert fnNum) &> Just)
    let newClosures   = Map.foldr upsert program.closures scope.environ
    let fn            = Function name argNames scope.environ fnNum scope.address
    let newFunctions  = Map.insert fnNum func program.functions
    modify $ \p -> p { closures = newClosures, functions = newFunctions, nextFnNum = fnNum + 1 }
    return fn
  where
    orEmpty setM = setM `orElse` Set.empty
    -- Possible optimization: Read through the statements and figure out which variables truly need to be closed over.
    -- Jason B. (12/25/25)

runFunction :: Evaluator -> FnID -> [Value] -> Evaluating
runFunction evaluator idNum args =
  do
    underlyingM    <- gets $ \s -> Map.lookup idNum s.functions
    let underlying  = underlyingM `orElse` (error whinerMsg)
    underlying evaluator args
  where
    whinerMsg = "Critical error!  You should never be able to run a non-existent function: " <> (showText idNum)
