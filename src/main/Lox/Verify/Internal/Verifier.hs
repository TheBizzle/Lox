module Lox.Verify.Internal.Verifier(verify) where

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
  , VerifierErrorType(CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers)
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type Validated t    = Validation (NonEmpty VerifierError) t
type InternalOutput = Validated ()
type Verification   = State ASTState InternalOutput

data ASTState =
  ASTState { canSuper :: Bool, classNames :: Set Text, isInClass :: Bool, vars :: Set Text, stack :: [Set Text] }
  deriving (Eq, Show)

initialState :: ASTState
initialState = ASTState False Set.empty False Set.empty []

verify :: AST -> Validated AST
verify ast = (evalState (findErrorInBlock ast.statements) initialState) *> (Success ast)

findErrorInStmt :: Statement -> Verification
findErrorInStmt (Block               stmts           ) = findErrorInBlock stmts
findErrorInStmt (Class               cVar   snM   ms ) = findErrorInClass cVar snM ms
findErrorInStmt (DeclareVar          decl   val      ) = findErrorInDecl decl val
findErrorInStmt (ExpressionStatement expr            ) = findErrorInExpr expr
findErrorInStmt (FunctionStatement   fn              ) = findErrorInFn fn
findErrorInStmt (IfElse              ante   is    esm) = findErrorInIf ante is esm
findErrorInStmt (PrintStatement      _      expr     ) = findErrorInExpr expr
findErrorInStmt (ReturnStatement     _      exprM    ) = maybe (return succeed) findErrorInExpr exprM
findErrorInStmt (WhileStatement      expr   bod      ) = findErrorInExpr expr |*> findErrorInStmt bod

findErrorInBlock :: [Statement] -> Verification
findErrorInBlock = (crawl findErrorInStmt) &> stackFrame

findErrorInClass :: Variable -> Maybe Variable -> [Function] -> Verification
findErrorInClass classVar superVarM methods =
  do
    result <- (methods |> helper &> superFrame &> classFrame &> stackFrame)
    registerClass classVar
    return result
  where
    helper = (crawl findErrorInFn)

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
    result3 <- maybe (return succeed) findErrorInStmt esm
    return $ result1 *> result2 *> result3

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
findErrorInExpr (VarRef      _             ) = return succeed

findErrorInThis :: TokenPlus -> Verification
findErrorInThis keyword =
  do
    isInClass <- gets isInClass
    if isInClass then do
      return succeed
    else
      return $ fail $ VerifierError CanOnlyRefThisInsideClass keyword

findErrorInSuper :: TokenPlus -> Verification
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

stackFrame :: Verification -> Verification
stackFrame fv = push *> fv <* pop
  where
    push = modify $ \s -> s { vars = Set.empty, stack = (s.vars):(s.stack) }
    pop  = modify $ \s ->
      case List.uncons s.stack of
        (Just (h, t)) -> s { vars = h, stack = t }
        Nothing       -> error "Tried to pop the top-level verification stack"

crawl :: (a -> Verification) -> [a] -> Verification
crawl f = foldr (\a b -> f a <*| b) $ return succeed

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

(<*|) :: (Monad m, Applicative a) => m (a x) -> m (a x) -> m (a x)
(<*|) = flip (|*>)
