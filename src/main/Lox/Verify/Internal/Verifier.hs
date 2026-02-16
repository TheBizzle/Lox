module Lox.Verify.Internal.Verifier(verify) where

import Control.Applicative(asum)
import Control.Monad.State(gets, modify)

import Data.List.NonEmpty(NonEmpty((:|)))

import Lox.Scanner.Token(TokenPlus)

import Lox.Parser.AST(
    AST(statements)
  , Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, VarRef)
  , Function(Function)
  , Literal(NilLit)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(Variable, varName, varToken)
  )

import Lox.Verify.Internal.VerifierError(
    VerifierError(VerifierError)
  , VerifierErrorType(DuplicateVar, ThisClassHasNoSupers)
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type Verification = State ASTState (Maybe VerifierError)

data ASTState =
  ASTState { canSuper :: Bool, classNames :: Set Text, isInClass :: Bool, vars :: Set Text, stack :: [Set Text] }
  deriving (Eq, Show)

verify :: AST -> Validation (NonEmpty VerifierError) AST
verify ast = ast.statements |>
               (\stmts -> evalState (findErrorInBlock stmts) (ASTState False Set.empty False Set.empty [])) &>
               maybe (Success ast) (NE.singleton &> Failure)

findErrorInStmt :: Statement -> Verification
findErrorInStmt (Block               stmts           ) = findErrorInBlock stmts
findErrorInStmt (Class               cVar   snM   ms ) = findErrorInClass cVar snM ms
findErrorInStmt (DeclareVar          decl   val      ) = findErrorInDecl decl val
findErrorInStmt (ExpressionStatement expr            ) = findErrorInExpr expr
findErrorInStmt (FunctionStatement   fn              ) = findErrorInFn fn
findErrorInStmt (IfElse              ante   is    esm) = findErrorInIf ante is esm
findErrorInStmt (PrintStatement      _      expr     ) = findErrorInExpr expr
findErrorInStmt (ReturnStatement     _      exprM    ) = maybe (return Nothing) findErrorInExpr exprM
findErrorInStmt (WhileStatement      expr   bod      ) = findErrorInExpr expr <||> findErrorInStmt bod

findErrorInBlock :: [Statement] -> Verification
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

findErrorInClass :: Variable -> Maybe Variable -> [Function] -> Verification
findErrorInClass classVar superVarM methods =
  do
    result <- (methods |> helper &> superFrame &> classFrame &> stackFrame)
    registerClass classVar
    return result
  where
    helper    [] = return Nothing
    helper (h:t) =
      do
        result <- findErrorInFn h
        if (not . isJust) result then
          helper t
        else
          return result

    superFrame fv = push *> fv <* pop
      where
        push = modify $ \s -> s { canSuper = isJust superVarM }
        pop  = modify $ \s -> s { canSuper = False }

    classFrame fv = push *> fv <* pop
      where
        push = modify $ \s -> s { isInClass =  True }
        pop  = modify $ \s -> s { isInClass = False }

    registerClass (Variable name _) =
      do
        modify $ \s -> s { classNames = Set.insert name s.classNames }

findErrorInDecl :: Variable -> Expr -> Verification
findErrorInDecl decl initial =
  do
    let vn  = decl.varName
    isDupe <- gets $ vars &> Set.member vn
    if not isDupe then do
      modify $ \s -> s { vars = Set.insert vn s.vars }
      findErrorInExpr initial
    else
      return $ errDupeVar decl

findErrorInFn :: Function -> Verification
findErrorInFn (Function _ ps stmts) = findErrorInBlock $ params <> stmts
  where
    params     = map asStmt ps
    asStmt var = DeclareVar var $ LiteralExpr NilLit var.varToken

findErrorInIf :: Expr -> Statement -> Maybe Statement -> Verification
findErrorInIf antecedent is esm =
  do
    result1 <- findErrorInExpr antecedent
    result2 <- findErrorInStmt is
    result3 <- maybe (return Nothing) findErrorInStmt esm
    return $ result1 <|> result2 <|> result3

findErrorInExpr :: Expr -> Verification
findErrorInExpr (Assign      _     val     ) = findErrorInExpr val
findErrorInExpr (Binary      l       _    r) = findErrorInExpr l <||> findErrorInExpr r
findErrorInExpr (Call        expr    _ args) = (traverse findErrorInExpr $ expr :| args) <&> asum
findErrorInExpr (Get         expr    _     ) = findErrorInExpr expr
findErrorInExpr (Grouping    expr          ) = findErrorInExpr expr
findErrorInExpr (LiteralExpr _       _     ) = return Nothing
findErrorInExpr (Logical     l       _    r) = findErrorInExpr l   <||> findErrorInExpr r
findErrorInExpr (Set         obj     _  val) = findErrorInExpr obj <||> findErrorInExpr val
findErrorInExpr (Super       kw    var     ) = findErrorInSuper kw var
findErrorInExpr (This        _             ) = return Nothing
findErrorInExpr (Unary       _    expr     ) = findErrorInExpr expr
findErrorInExpr (VarRef      _             ) = return Nothing

findErrorInSuper :: TokenPlus -> Variable -> Verification
findErrorInSuper keyword _ =
  do
    canSuper <- gets canSuper
    if canSuper then
      return Nothing
    else
      return $ Just $ VerifierError ThisClassHasNoSupers keyword

stackFrame :: Verification -> Verification
stackFrame fv = push *> fv <* pop
  where
    push = modify $ \s -> s { vars = Set.empty, stack = (s.vars):(s.stack) }
    pop  = modify $ \s ->
      case List.uncons s.stack of
        (Just (h, t)) -> s { vars = h, stack = t }
        Nothing       -> error "Tried to pop the top-level verification stack"

errDupeVar :: Variable -> Maybe VerifierError
errDupeVar = varToken &> VerifierError DuplicateVar &> Just

(<||>) :: (Monad m, Alternative a) => m (a x) -> m (a x) -> m (a x)
(<||>) a b = (<|>) <$> a <*> b
