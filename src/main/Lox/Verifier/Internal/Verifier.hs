module Lox.Verifier.Internal.Verifier(verify) where

import Lox.Parser.AST(AST(statements))

import Lox.Verifier.Internal.BlockVerifier(findErrorInBlock)
import Lox.Verifier.Internal.Common(ASTState(ASTState), Validated)

import qualified Data.Set as Set


verify :: AST -> Validated AST
verify ast = (evalState (findErrorInBlock ast.statements) initialState) *> (Success ast)
  where
    initialState = ASTState False Set.empty False False False [] Nothing Set.empty
