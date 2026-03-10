module Lox.Verifier.Internal.Verifier(verify) where

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

import Lox.Verifier.Internal.VerifierError(
    VerifierError(VerifierError)
  , VerifierErrorType(CannotInheritFromSelf, CannotReturnAtTopLevel, CannotReturnInConstructor, CanOnlyRefSuperInsideClass, CanOnlyRefThisInsideClass, DuplicateVar, ThisClassHasNoSupers, VarCannotInitInTermsOfSelf)
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


type Validated t    = Validation (NonEmpty VerifierError) t
type InternalOutput = Validated ()
type Verification   = State ASTState InternalOutput

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

initialState :: ASTState
initialState = ASTState False Set.empty False False False [] Nothing Set.empty

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
findErrorInStmt (ReturnStatement     kw     exprM    ) = findErrorInReturn kw exprM
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
    helper fn = findSuperError |*> (crawl findErrorInFn fn)

    findSuperError =
      return $
        case superVarM of
          Nothing       -> succeed
          Just superVar ->
            if classVar.varName == superVar.varName then
              fail $ VerifierError CannotInheritFromSelf classVar.varToken
            else
              succeed

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
    let vn    = decl.varName
    isDupe   <- gets $ vars  &> Set.member vn
    isGlobal <- gets $ stack &> List.length &> (== 1)
    if (not isDupe) || isGlobal then do
      oldInitter <- gets varDoingInit
      modify $ \s -> s { vars = Set.insert vn s.vars, varDoingInit = Just decl.varName }
      res <- findErrorInExpr initial
      modify $ \s -> s { varDoingInit = oldInitter }
      return res
    else
      return $ errDupeVar decl

findErrorInFn :: Function -> Verification
findErrorInFn (Function decl ps stmts) =
    do
      isClass <- gets isInClass
      isFn    <- gets isInFunction
      modify $ \s -> s { isInConstructor = isClass && decl.varName == "init", isInFunction = True }
      res <- findErrorInBlock $ params <> stmts
      modify $ \s -> s { isInConstructor = False, isInFunction = isFn }
      return res
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

findErrorInReturn :: TokenPlus -> Maybe Expr -> Verification
findErrorInReturn keyword exprM = findErrorInCtor |*> findErrorInRaw |*> maybe (return succeed) findErrorInExpr exprM
  where
    findErrorInCtor =
      do
        isInCtor <- gets isInConstructor
        if isInCtor && isJust exprM then
          return $ fail $ VerifierError CannotReturnInConstructor keyword
        else
          return succeed

    findErrorInRaw =
      do
        isInFn <- gets isInFunction
        if not isInFn then
          return $ fail $ VerifierError CannotReturnAtTopLevel keyword
        else
          return succeed

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
crawl f = map f &> (foldl (|*>) $ return succeed)

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
