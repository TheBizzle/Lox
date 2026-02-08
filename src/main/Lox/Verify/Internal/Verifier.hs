module Lox.Verify.Internal.Verifier(verify) where

import Control.Monad.State(gets, modify)

import Lox.Parser.AST(
    AST(statements)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, Function, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(varName, varToken)
  )

import Lox.Verify.Internal.VerifierError(VerifierError(VerifierError), VerifierErrorType(DuplicateVar))

import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type FnVerification = State FnState (Maybe VerifierError)

data FnState =
  FnState { vars :: Set Text }
  deriving (Eq, Show)

verify :: AST -> Validation (NonEmpty VerifierError) AST
verify ast = ast.statements |>
               (foldr (\x acc -> findErrorInStmt x <|> acc) Nothing) &>
               maybe (Success ast) (NE.singleton &> Failure)

findErrorInStmt :: Statement -> Maybe VerifierError
findErrorInStmt (Block               _       ) = Nothing
findErrorInStmt (Class               _  _   _) = Nothing
findErrorInStmt (DeclareVar          _  _    ) = Nothing
findErrorInStmt (ExpressionStatement _       ) = Nothing
findErrorInStmt (Function            _ ps bod) = evalState (findErrorInFn ps bod) $ FnState Set.empty
findErrorInStmt (IfElse              _  _   _) = Nothing
findErrorInStmt (PrintStatement      _  _    ) = Nothing
findErrorInStmt (ReturnStatement     _  _    ) = Nothing
findErrorInStmt (WhileStatement      _  _    ) = Nothing

findErrorInFn :: [Variable] -> [Statement] -> FnVerification
findErrorInFn vs stmts =
  do
    result <- finder vs
    if isJust result then
      return result
    else
      findErrorInStmts stmts
  where
    finder    [] = return Nothing
    finder (v:t) =
      do
        isDeclared <- gets $ vars &> Set.member v.varName
        if isDeclared then
          return $ Just $ VerifierError DuplicateVar v.varToken
        else do
          modify $ \fns -> fns { vars = Set.insert v.varName fns.vars }
          finder t

findErrorInStmts :: [Statement] -> FnVerification
findErrorInStmts _ = return Nothing
