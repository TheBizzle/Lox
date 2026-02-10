module Lox.Verify.Internal.Verifier(verify) where

import Control.Monad.State(gets, modify)

import Lox.Parser.AST(
    AST(statements)
  , Expr(LiteralExpr)
  , Function(Function)
  , Literal(NilLit)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(varName, varToken)
  )

import Lox.Verify.Internal.VerifierError(VerifierError(VerifierError), VerifierErrorType(DuplicateVar))

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type FnVerification = State FnState (Maybe VerifierError)

data FnState =
  FnState { vars :: Set Text, stack :: [Set Text] }
  deriving (Eq, Show)

verify :: AST -> Validation (NonEmpty VerifierError) AST
verify ast = ast.statements |>
               (\stmts -> evalState (findErrorInBlock stmts) (FnState Set.empty [])) &>
               maybe (Success ast) (NE.singleton &> Failure)

findErrorInStmt :: Statement -> FnVerification
findErrorInStmt (Block               stmts         ) = findErrorInBlock stmts
findErrorInStmt (Class               _      _   ms ) = findErrorInClass ms
findErrorInStmt (DeclareVar          decl   _      ) = findErrorInDecl decl
findErrorInStmt (ExpressionStatement _             ) = return Nothing
findErrorInStmt (FunctionStatement   fn            ) = findErrorInFn fn
findErrorInStmt (IfElse              _      is  esm) = findErrorInIf is esm
findErrorInStmt (PrintStatement      _      _      ) = return Nothing
findErrorInStmt (ReturnStatement     _      _      ) = return Nothing
findErrorInStmt (WhileStatement      _      bod    ) = findErrorInStmt bod

findErrorInBlock :: [Statement] -> FnVerification
findErrorInBlock = helper &> stackFrame
  where
    helper    [] = return Nothing
    helper (h:t) =
      do
        result <- findErrorInStmt h
        if (not . isJust) result then
          helper t
        else
          return result

findErrorInClass :: [Function] -> FnVerification
findErrorInClass = helper &> stackFrame
  where
    helper    [] = return Nothing
    helper (h:t) =
      do
        result <- findErrorInFn h
        if (not . isJust) result then
          helper t
        else
          return result

findErrorInDecl :: Variable -> FnVerification
findErrorInDecl decl =
  do
    let vn  = decl.varName
    isDupe <- gets $ vars &> Set.member vn
    if not isDupe then do
      modify $ \s -> s { vars = Set.insert vn s.vars }
      return Nothing
    else
      return $ errDupeVar decl

findErrorInFn :: Function -> FnVerification
findErrorInFn (Function _ ps stmts) = findErrorInBlock $ params <> stmts
  where
    params     = map asStmt ps
    asStmt var = DeclareVar var $ LiteralExpr NilLit var.varToken

findErrorInIf :: Statement -> Maybe Statement -> FnVerification
findErrorInIf is esm =
  do
    result1 <- findErrorInStmt is
    result2 <- maybe (return Nothing) findErrorInStmt esm
    return $ result1 <|> result2

stackFrame :: FnVerification -> FnVerification
stackFrame fv = push *> fv <* pop
  where
    push = modify $ \s -> s { vars = Set.empty, stack = (s.vars):(s.stack) }
    pop  = modify $ \s ->
      case List.uncons s.stack of
        (Just (h, t)) -> s { vars = h, stack = t }
        Nothing       -> error "Tried to pop the top-level verification stack"

errDupeVar :: Variable -> Maybe VerifierError
errDupeVar = varToken &> VerifierError DuplicateVar &> Just
