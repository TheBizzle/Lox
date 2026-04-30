module Lox.Verifier.Internal.Common(
    (|*>)
  , ASTState(ASTState, canSuper, classNames, isInClass, isInConstructor, isInFunction, stack, varDoingInit, vars)
  , errDupeVar, fail, succeed, Validated, Verification
  ) where

import Lox.Parser.AST(Variable(varToken))

import Lox.Verifier.Internal.VerifierError(
    VerifierError(VerifierError)
  , VerifierErrorType(DuplicateVar)
  )

import qualified Data.List.NonEmpty as NE


data ASTState =
  ASTState { canSuper        :: Bool
           , classNames      :: Set Text
           , isInClass       :: Bool
           , isInConstructor :: Bool
           , isInFunction    :: Bool
           , stack           :: [Set Text]
           , varDoingInit    :: Maybe Text
           , vars            :: Set Text
           }
  deriving (Eq, Show)

type Validated t    = Validation (NonEmpty VerifierError) t
type InternalOutput = Validated ()
type Verification   = State ASTState InternalOutput

errDupeVar :: Variable -> InternalOutput
errDupeVar = varToken &> VerifierError DuplicateVar &> fail

fail :: VerifierError -> InternalOutput
fail = NE.singleton &> Failure

succeed :: InternalOutput
succeed = Success ()

(|*>) :: (Monad m, Applicative a) => m (a x) -> m (a x) -> m (a x)
(|*>) a b =
  do
    va <- a
    vb <- b
    return $ va *> vb
