module Lox.Verify.Internal.Verifier(verify) where

import Data.Foldable(asum)

import Lox.Parser.AST(
    AST(statements)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, Function, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  )

import Lox.Verify.Internal.VerifierError(VerifierError)

import qualified Data.List.NonEmpty as NE

verify :: AST -> Validation (NonEmpty VerifierError) AST
verify ast = ast.statements |>
               (foldr (\x acc -> findErrorInStmt x <|> acc) Nothing) &>
               maybe (Success ast) (NE.singleton &> Failure)

findErrorInStmt :: Statement -> Maybe VerifierError
findErrorInStmt (Block               _ _ _  ) = Nothing
findErrorInStmt (Class               _ _ _ _) = Nothing
findErrorInStmt (DeclareVar          _ _ _  ) = Nothing
findErrorInStmt (ExpressionStatement _ _ _  ) = Nothing
findErrorInStmt (Function            _ _ _  ) = Nothing
findErrorInStmt (IfElse              _ _ _  ) = Nothing
findErrorInStmt (PrintStatement      _ _ _  ) = Nothing
findErrorInStmt (ReturnStatement     _ _ _  ) = Nothing
findErrorInStmt (WhileStatement      _ _ _  ) = Nothing
