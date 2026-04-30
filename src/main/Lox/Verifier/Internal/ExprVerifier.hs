module Lox.Verifier.Internal.ExprVerifier(findErrorInExpr) where

import Control.Monad.State(gets)

import Data.List.NonEmpty(NonEmpty((:|)))

import Lox.Parser.AST(Variable(varName, varToken))

import Lox.Scanner.Token(Token)

import Lox.Parser.AST(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, VarRef)
  )

import Lox.Verifier.Internal.Common(
    (|*>)
  , ASTState(canSuper, isInClass, stack, varDoingInit)
  , fail, succeed, Verification
  )

import Lox.Verifier.Internal.VerifierError(
    VerifierError(VerifierError)
  , VerifierErrorType(CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, ThisClassHasNoSupers, VarCannotInitInTermsOfSelf)
  )

import qualified Data.List as List


findErrorInExpr :: Expr -> Verification
findErrorInExpr (Assign      _     val     ) = findErrorInExpr val
findErrorInExpr (Binary      l       _    r) = findErrorInExpr l |*> findErrorInExpr r
findErrorInExpr (Call        expr    _ args) = (traverse findErrorInExpr $ expr :| args) <&> (foldr1 (<*))
findErrorInExpr (Get         expr    _     ) = findErrorInExpr expr
findErrorInExpr (Grouping    expr          ) = findErrorInExpr expr
findErrorInExpr (LiteralExpr _       _     ) = return succeed
findErrorInExpr (Logical     l       _    r) = findErrorInExpr l   |*> findErrorInExpr r
findErrorInExpr (Set         obj     _  val) = findErrorInExpr obj |*> findErrorInExpr val
findErrorInExpr (Super       kw      _     ) = findErrorInSuper kw
findErrorInExpr (This        kw            ) = findErrorInThis kw
findErrorInExpr (Unary       _    expr     ) = findErrorInExpr expr
findErrorInExpr (VarRef      var           ) = findErrorInVarRef var

findErrorInVarRef :: Variable -> Verification
findErrorInVarRef var =
  do
    current  <- gets varDoingInit
    isGlobal <- gets $ stack &> List.length &> (== 1)
    let notG  = not isGlobal
    case current of
      (Just vn) | vn == var.varName && notG -> return $ fail $ VerifierError VarCannotInitInTermsOfSelf var.varToken
      _                                     -> return succeed

findErrorInThis :: Token -> Verification
findErrorInThis keyword =
  do
    isInClass <- gets isInClass
    if isInClass then do
      return succeed
    else
      return $ fail $ VerifierError CanOnlyRefThisInsideClass keyword

findErrorInSuper :: Token -> Verification
findErrorInSuper keyword =
  do
    isInClass <- gets isInClass
    canSuper  <- gets canSuper
    return $
      if not isInClass then
        fail $ VerifierError CanOnlyRefSuperInsideClass keyword
      else if not canSuper then
        fail $ VerifierError ThisClassHasNoSupers keyword
      else
        succeed
