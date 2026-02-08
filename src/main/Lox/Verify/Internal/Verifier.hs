module Lox.Verify.Internal.Verifier(verify) where

import Control.Monad.State(gets, modify)

import Lox.Parser.AST(
    AST(statements)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, Function, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(varName, varToken)
  )

import Lox.Verify.Internal.VerifierError(VerifierError(VerifierError), VerifierErrorType(DuplicateVar))

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type FnVerification = State FnState (Maybe VerifierError)

data FnState =
  FnState { vars :: Set Text }
  deriving (Eq, Show)

verify :: AST -> Validation (NonEmpty VerifierError) AST
verify ast = ast.statements |>
               (foldr (\x acc -> (evalState (findErrorInStmt x) (FnState Set.empty)) <|> acc) Nothing) &>
               maybe (Success ast) (NE.singleton &> Failure)

findErrorInStmt :: Statement -> FnVerification
findErrorInStmt (Block               _       ) = return Nothing
findErrorInStmt (Class               _  _   _) = return Nothing
findErrorInStmt (DeclareVar          _  _    ) = return Nothing
findErrorInStmt (ExpressionStatement _       ) = return Nothing
findErrorInStmt (Function            _ ps bod) = findErrorInFn ps bod
findErrorInStmt (IfElse              _  _   _) = return Nothing
findErrorInStmt (PrintStatement      _  _    ) = return Nothing
findErrorInStmt (ReturnStatement     _  _    ) = return Nothing
findErrorInStmt (WhileStatement      _  _    ) = return Nothing

findErrorInFn :: [Variable] -> [Statement] -> FnVerification
findErrorInFn vs stmts =
  do
    let (_, result) = foldr func (Set.empty, Nothing) $ List.reverse vs
    if isJust result then
      return result
    else
      findErrorInStmts stmts
  where
    func _ (names, (Just x)) = (names, Just x)
    func v (names,        _) =
      if Set.member v.varName names then
        (names, Just $ VerifierError DuplicateVar v.varToken)
      else
        (Set.insert v.varName names, Nothing)

findErrorInStmts :: [Statement] -> FnVerification
findErrorInStmts    [] = return Nothing
findErrorInStmts (h:t) =
  do
    result <- findErrorInStmt h
    if isJust result then
      return result
    else
      findErrorInStmts t
