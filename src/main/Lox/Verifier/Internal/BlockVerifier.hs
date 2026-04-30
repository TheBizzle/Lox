module Lox.Verifier.Internal.BlockVerifier(findErrorInBlock) where

import Control.Monad.State(gets, modify)

import Lox.Scanner.Token(Token)

import Lox.Parser.AST(
    Expr(LiteralExpr)
  , Function(Function)
  , Literal(NilLit)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(Variable, varName, varToken)
  )

import Lox.Verifier.Internal.Common(
    (|*>)
  , ASTState(canSuper, classNames, isInClass, isInConstructor, isInFunction, stack, varDoingInit, vars)
  , errDupeVar, fail, succeed, Verification
  )

import Lox.Verifier.Internal.ExprVerifier(findErrorInExpr)

import Lox.Verifier.Internal.VerifierError(
    VerifierError(VerifierError)
  , VerifierErrorType(CannotInheritFromSelf, CannotReturnAtTopLevel, CannotReturnInConstructor)
  )

import qualified Data.List as List
import qualified Data.Set  as Set


findErrorInBlock :: [Statement] -> Verification
findErrorInBlock = (crawl findErrorInStmt) &> stackFrame

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

      isClass   <- gets isInClass
      wasFn     <- gets isInFunction
      wasInCtor <- gets isInConstructor

      modify $ \s -> s { isInConstructor = isClass && decl.varName == "init" && not wasInCtor, isInFunction = True }
      res <- findErrorInBlock $ params <> stmts
      modify $ \s -> s { isInConstructor = wasInCtor, isInFunction = wasFn }

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

findErrorInReturn :: Token -> Maybe Expr -> Verification
findErrorInReturn keyword exprM =
    findErrorInCtor |*> findErrorInRaw |*> maybe (return succeed) findErrorInExpr exprM
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

crawl :: (a -> Verification) -> [a] -> Verification
crawl f = map f &> (foldl (|*>) $ return succeed)

stackFrame :: Verification -> Verification
stackFrame fv = push *> fv <* pop
  where
    push = modify $ \s -> s { vars = Set.empty, stack = (s.vars):(s.stack) }
    pop  = modify $ \s ->
      case List.uncons s.stack of
        (Just (h, t)) -> s { vars = h, stack = t }
        Nothing       -> error "Tried to pop the top-level verification stack"
