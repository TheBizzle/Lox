module Lox.Evaluator.Internal.Eval.Statement(runStatements) where

import Control.Monad.State(gets, modify)

import Lox.Scanner.Token(Token)

import Lox.Parser.AST(
    Expr
  , Function(Function)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(Variable, varName)
  )

import Lox.Evaluator.Internal.EvalError(
    EvalError(EvalError)
  , EvalErrorType(SuperMustBeAClass, TopLevelReturn, UnknownVariable)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print), runEffect)
import Lox.Evaluator.Internal.Function(defineFunction)
import Lox.Evaluator.Internal.GC(transferOwnership)
import Lox.Evaluator.Internal.OOP(defineClass)
import Lox.Evaluator.Internal.Program(Evaluating)
import Lox.Evaluator.Internal.Value(Value(ClassV, Nada, NilV))
import Lox.Evaluator.Internal.Variable(currentEnvironment, declareVar, getVar, popScope, pushScope)

import Lox.Evaluator.Internal.Eval.Common(
    asBool, lose, nothing, onSuccessEval, succeed, win
  )

import Lox.Evaluator.Internal.Eval.Expr(evalExpr)

import qualified Lox.Evaluator.Internal.ControlFlow as CF


runStatements :: [Statement] -> Evaluating
runStatements = foldM helper $ succeed Nada
  where
    helper acc s = acc `onSuccessEval` (const $ evalStatement s)

evalStatement :: Statement -> Evaluating
evalStatement (Block               statements           ) = evalBlock statements
evalStatement (Class               var        stmts ms  ) = evalClass var stmts ms
evalStatement (DeclareVar          var        expr      ) = evalDeclaration var expr
evalStatement (ExpressionStatement                  expr) = evalExpr evalStatement expr
evalStatement (FunctionStatement   func                 ) = evalFunction func
evalStatement (IfElse              ant        con   alt ) = evalIfElse ant con alt
evalStatement (PrintStatement      _                expr) = evalPrint expr
evalStatement (ReturnStatement     token      expM      ) = evalReturn token expM
evalStatement (WhileStatement      pred       stmt      ) = evalWhile pred stmt

evalBlock :: [Statement] -> Evaluating
evalBlock statements =
  do
    env <- currentEnvironment
    modify $ pushScope env
    result <- runStatements statements
    modify popScope
    return result

evalClass :: Variable -> Maybe Variable -> [Function] -> Evaluating
evalClass (Variable className _) superNameTokenM methods =
  do
    superClassMV <- processSuperVar superNameTokenM
    superClassMV `failOrM` (defClass methods)
  where
    processSuperVar Nothing                                          = return $ Success Nothing
    processSuperVar (Just (Variable name     _)) | name == className = error "`super` cannot be self.  Verifier should have caught this."
    processSuperVar (Just (Variable name token))                     =
      do
        valM <- gets $ getVar name
        maybe (lose $ EvalError (UnknownVariable name) token) processSuperValue valM
      where
        processSuperValue (ClassV clazz) = return $ Success $ Just clazz
        processSuperValue              _ = lose $ EvalError (SuperMustBeAClass name) token

    defClass methods superClassM =
      do
        void $ defineClass className superClassM methods
        nothing

evalDeclaration :: Variable -> Expr -> Evaluating
evalDeclaration (Variable varName _) expr =
  do
    valueV <- evalExpr evalStatement expr
    valueV `onSuccessEval` (
        \value -> do
          modify $ declareVar varName value
          nothing
      )

evalFunction :: Function -> Evaluating
evalFunction (Function var params body) =
  do
    void $ defineFunction var.varName pNames body
    nothing
  where
    pNames = map varName params

evalIfElse :: Expr -> Statement -> (Maybe Statement) -> Evaluating
evalIfElse antecedentExpr consequent alternativeM =
  do
    anteV <- evalExpr evalStatement antecedentExpr
    anteV `onSuccessEval` (
      \antecedent ->
        if (asBool antecedent) then
          evalStatement consequent
        else
          maybe nothing evalStatement alternativeM
      )

evalPrint :: Expr -> Evaluating
evalPrint expr =
  do
    valueV <- evalExpr evalStatement expr
    valueV `onSuccessEval` (
      \value -> do
        let text = showText $ if value == Nada then NilV else value
        void $ runEffect $ Print text
        nothing
      )

evalReturn :: Token -> Maybe Expr -> Evaluating
evalReturn token exprM =
    maybe (return $ Success $ CF.Return token Nada) (evalExpr evalStatement >=> helper) exprM
  where
    helper = flip failOrM $ \case
      ex@(CF.Exception _) -> return $ Success ex
      (   CF.Normal    v) -> (transferOwnership v) $> (Success $ CF.Return token v)
      (   CF.Return  _ _) -> error "`return return x;` should not be possible!"

evalWhile :: Expr -> Statement -> Evaluating
evalWhile predExpr body =
  do
    predV <- evalExpr evalStatement predExpr
    predV `onSuccessEval` (
      \predicate ->
        do
          if asBool predicate then do
            resultV <- evalStatement body
            resultV `onSuccessEval` (const $ evalWhile predExpr body)
          else
            win Nada
      )
