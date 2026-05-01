module Lox.Evaluator.Internal.GC(borrowIfFn, cleanupScope, transferOwnership) where

import Control.Monad.State(get, modify)

import Lox.Evaluator.Internal.Program(
    Program
  , ProgramState(closures, fnBorrowers, functions, scopes, variables)
  )

import Lox.Evaluator.Internal.Scope(
    Environment, Scope(environ), ScopeAddress
  , VarAddress(VarAddress)
  , Scope(address)
  , VarAddress(scopeAddr, VarAddress)
  )

import Lox.Evaluator.Internal.Value(
    Function(idNum, name)
  , Value(function, FunctionV)
  )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import qualified Data.Set           as Set


borrowIfFn :: ScopeAddress -> Value -> Program ()
borrowIfFn address (FunctionV _ fn _) = borrowFn address fn
borrowIfFn       _                  _ = return ()

borrowFn :: ScopeAddress -> Function -> Program ()
borrowFn address fn =
  do
    prog            <- get
    let newXferreds  = Map.alter insertOrSingleton fn.idNum prog.fnBorrowers
    modify $ \p -> p { fnBorrowers = newXferreds }
  where
    insertOrSingleton (Just set) = Just $ Set.insert    newAddr set
    insertOrSingleton    Nothing = Just $ Set.singleton newAddr
    newAddr                      = VarAddress fn.name address

cleanupScope :: Scope -> ProgramState -> ProgramState
cleanupScope scope program = program { variables = cleanedVars, closures = cleanedClosures, functions = cleanedFunctions, fnBorrowers = cleanedXFns }
  where
    locals :: Environment
    locals          = Map.filter (scopeAddr &> (== scope.address)) scope.environ
    (fAs, xAs)      = Map.partition checkIsFunction     locals
    (tAs, dyingFAs) = Map.partition checkWasTransferred fAs

    -- Clean up all of my local vars that haven't been closed over
    closedOverAs = Map.keysSet $ Map.filter (not . Set.null) program.closures
    dyingVAs     = (Set.fromList $ Map.elems xAs) `Set.difference` closedOverAs
    cleanedVars  = Set.foldr Map.delete program.variables dyingVAs

    -- If it was transferred to me, remove it from the list of transferreds
    xedToMeFns  = Map.filter (scopeAddr &> (== scope.address)) tAs
    cleanedXFns = foldr unborrow program.fnBorrowers $ Map.elems xedToMeFns

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
        isFunction (FunctionV _ _ _) = True
        isFunction                 _ = False
        noBueno                      = "You can't possibly be cleaning up a variable that's already been cleaned up...."

    checkWasTransferred = checkVar wasTransferred noBueno
      where
        wasTransferred (FunctionV _ fn _) = maybe False (not . Set.null) $ Map.lookup fn.idNum program.fnBorrowers
        wasTransferred                  _ = error "Not possible!  We already proved these are only functions!"
        noBueno                           = "You can't possibly be checking a function that's already been cleaned up...."

    toFnID = checkVar (function &> idNum) "You can't possibly be getting the FnID of a function that's already been cleaned up...."

    unborrow varID borroweds = Map.alter f (toFnID varID) borroweds
      where
        f (Just set) = Just $ Set.delete varID set
        f    Nothing = error "Shouldn't be possible to unborrow something that's not borrowed."

    checkVar :: (Value -> a) -> Text -> VarAddress -> a
    checkVar conversion errorMsg = (flip Map.lookup program.variables) &> maybe (error errorMsg) conversion

transferOwnership :: Value -> Program ()
transferOwnership value =
  do
    prog           <- get
    let parentAddr  = (neSecond prog.scopes).address
    borrowIfFn parentAddr value
  where
    neSecond ne =
      case NE.tail ne of
        (h:_) -> h
        []    -> error "Not possible!  You can't call `return` from the top scope!"

pruneSet :: Ord a => Set a -> Set a -> Maybe (Set a)
pruneSet baddies current =
  if not $ Set.null res then
    Just res
  else
    Nothing
  where
    res = current `Set.difference` baddies
