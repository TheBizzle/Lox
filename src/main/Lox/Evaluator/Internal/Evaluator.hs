module Lox.Evaluator.Internal.Evaluator(eval) where

import Control.Monad.State(get, gets, modify)

import Lox.Parser.AST(
    AST(statements)
  , Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, VarRef)
  , exprToToken
  , Function(fnDecl, Function)
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Statement(Block, Class, DeclareVar, ExpressionStatement, FunctionStatement, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  , Variable(Variable, varName)
  )

import Lox.Scanner.Token(
    Token(Token, typ)
  , TokenType(And, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.Value(Value(BooleanV, ClassV, FunctionV, Nada, NilV, NumberV, ObjectV, StringV))

import Lox.Evaluator.Internal.EvalError(
    EvalError(EvalError)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlySetObj, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, NotCallable, SuperMustBeAClass, TopLevelReturn, UnknownVariable)
  )

import Lox.Evaluator.Internal.Program(
    arity, assignIntoObject, currentEnvironment, declareVar, defineClass, defineFunction, failOrM, getVar, indexObject, indexSuper, initObject, popScope, Program, pushScope, runEffect, runFunction, setVar, transferOwnership
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE

import qualified Lox.Evaluator.Internal.ControlFlow as CF
import qualified Lox.Evaluator.Internal.Value       as V


type Result t   = Validation (NonEmpty EvalError) t
type Prog   t   = Program (Result t)
type Evaluated  = Prog Value
type Evaluating = Prog CF.ControlFlow

eval :: AST -> Evaluated
eval = statements &> runTopLevelStatements &> evaluated

evaluated :: Evaluating -> Evaluated
evaluated = (<&> (validation Failure helper))
  where
    helper (CF.Exception    ex   ) = Failure $ NE.singleton ex
    helper (CF.Normal    value   ) = Success value
    helper (CF.Return    token  _) = Failure $ NE.singleton $ EvalError TopLevelReturn token

evalStatement :: Statement -> Evaluating
evalStatement (Block               statements           ) = evalBlock statements
evalStatement (Class               var        stmts ms  ) = evalClass var stmts ms
evalStatement (DeclareVar          var        expr      ) = evalDeclaration var expr
evalStatement (ExpressionStatement                  expr) = evalExpr expr
evalStatement (FunctionStatement   func                 ) = evalFunction func
evalStatement (IfElse              ant        con   alt ) = evalIfElse ant con alt
evalStatement (PrintStatement      _                expr) = evalPrint expr
evalStatement (ReturnStatement     token      expM      ) = evalReturn token expM
evalStatement (WhileStatement      pred       stmt      ) = evalWhile pred stmt

evalExpr :: Expr -> Evaluating
evalExpr (Assign      var value)              = evalAssign var value
evalExpr (Binary      left operator right)    = evalBinary left operator right
evalExpr (Call        callee token arguments) = evalCall callee token arguments
evalExpr (Get         object var)             = evalGet object var
evalExpr (Grouping    expression)             = evalExpr expression
evalExpr (LiteralExpr literal _)              = win $ evalLiteral literal
evalExpr (Logical     left operator right)    = evalLogical left operator right
evalExpr (Set         object var value)       = evalSet object var value
evalExpr (Super       keyword var)            = indexSuper keyword var
evalExpr (This        keyword)                = evalThis keyword
evalExpr (Unary       operator right)         = evalUnary operator right
evalExpr (VarRef      var)                    = lookupVar var

evalAssign :: Variable -> Expr -> Evaluating
evalAssign var value = (evalExpr value) >>= (flip onSuccessEval $ \v -> setVariable var v)

evalBinary :: Expr -> Token -> Expr -> Evaluating
evalBinary left operator right =
    do
      lv <- evalExpr left
      rv <- evalExpr right
      ((,) <$> lv <*> rv) `onSuccessEval2` (
        \(l, r) -> return $ helper l operator.typ r
        )
  where
    helper l           BangEqual    r           = bool l (/=) r
    helper l           EqualEqual   r           = bool l (==) r
    helper (NumberV l) Greater      (NumberV r) = bool l (> ) r
    helper (NumberV l) GreaterEqual (NumberV r) = bool l (>=) r
    helper (NumberV l) Less         (NumberV r) = bool l (< ) r
    helper (NumberV l) LessEqual    (NumberV r) = bool l (<=) r
    helper (NumberV l) Minus        (NumberV r) = num  l (- ) r
    helper (NumberV l) Plus         (NumberV r) = num  l (+ ) r
    helper (StringV l) Plus         (StringV r) = str  l (<>) r
    helper _           Plus         _           = fail $ EvalError OperandsMustBeNumsOrStrs operator
    helper (NumberV l) Slash        (NumberV r) = num  l (/#) r
    helper (NumberV l) Star         (NumberV r) = num  l (* ) r
    helper _           _            _           = fail $ EvalError OperandsMustBeNumbers operator

    bool = succ BooleanV
    num  = succ NumberV
    str  = succ StringV

    succ consV l op r = Success $ CF.Normal $ consV $ l `op` r

evalBlock :: [Statement] -> Evaluating
evalBlock statements =
  do
    env <- currentEnvironment
    modify $ pushScope env
    result <- runStatements statements
    modify popScope
    return result

evalCall :: Expr -> Token -> [Expr] -> Evaluating
evalCall callableExpr token argExprs =
  do
    callableV <- evalExpr callableExpr
    argVs     <- forM argExprs evalExpr
    ((,) <$> callableV <*> (sequenceA argVs)) `onSuccessEval2Seq` helper
  where
    helper (callableV, args) =
      case (arity callableV, callableV) of
        (Nothing  ,                 _)                         -> lose $ EvalError NotCallable $ exprToToken callableExpr
        (Just arty,                 _) | doesntMatch arty args -> lose $ EvalError (ArityMismatch arty $ numGotten args) $ exprToToken callableExpr
        (        _, FunctionV  _ fn _)                         -> runFunction       evalStatement fn.idNum    args
        (        _, ClassV      clazz)                         -> initObject  token evalStatement clazz.cName args
        x                                                      -> error $ "This isn't the callable we're looking for: " <> (showText x)

    doesntMatch arity args = arity /= (numGotten args)

    numGotten args = args |> length &> fromIntegral

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

evalFunction :: Function -> Evaluating
evalFunction (Function var params body) =
  do
    void $ defineFunction var.varName pNames body
    nothing
  where
    pNames = map varName params

evalGet :: Expr -> Variable -> Evaluating
evalGet objectExpr (Variable propName token) =
  do
    valueV <- evalExpr objectExpr
    valueV `onSuccessEval` (
        \v -> (asObject v $ EvalError CanOnlyGetObj token) `failOrM` (\x -> indexObject token x propName)
      )

evalIfElse :: Expr -> Statement -> (Maybe Statement) -> Evaluating
evalIfElse antecedentExpr consequent alternativeM =
  do
    anteV <- evalExpr antecedentExpr
    anteV `onSuccessEval` (
      \antecedent ->
        if (asBool antecedent) then
          evalStatement consequent
        else
          maybe nothing evalStatement alternativeM
      )

evalLiteral :: Literal -> Value
evalLiteral (BooleanLit bool)   = BooleanV bool
evalLiteral (DoubleLit  double) =  NumberV double
evalLiteral (StringLit  str)    =  StringV str
evalLiteral  NilLit             =     NilV

evalLogical :: Expr -> Token -> Expr -> Evaluating
evalLogical left operator right =
  do
    lv <- evalExpr left
    lv `onSuccessEval` (
      \l -> case operator of
              (Token And _) -> if (not . asBool $ l) then return (succeed l) else evalExpr right
              (Token  Or _) -> if (      asBool   l) then return (succeed l) else evalExpr right
              token         -> error $ "Impossible logical operator: " <> (showText token)
      )

evalReturn :: Token -> Maybe Expr -> Evaluating
evalReturn token exprM = maybe (return $ Success $ CF.Return token Nada) (evalExpr >=> helper) exprM
  where
    helper = flip failOrM $ \case
      ex@(CF.Exception _) -> return $ Success ex
      (   CF.Normal    v) -> (transferOwnership v) $> (Success $ CF.Return token v)
      (   CF.Return  _ _) -> error "`return return x;` should not be possible!"

evalSet :: Expr -> Variable -> Expr -> Evaluating
evalSet objectExpr (Variable propName token) valueExpr =
  do
    objValV <- evalExpr objectExpr
    anyValV <- evalExpr  valueExpr
    ((,) <$> objValV <*> anyValV) `onSuccessEval2` (
        \(objVal, anyVal) -> (asObject objVal $ EvalError CanOnlySetObj token) `failOrM` (\obj -> assignIntoObject obj propName anyVal)
      )

evalThis :: Token -> Evaluating
evalThis _ = get >>= ((getVar "this") &> maybe (error "Can't reference `this` outside of class.  Verifier should have caught this!") win)

evalUnary :: Token -> Expr -> Evaluating
evalUnary operator rightExpr =
  do
    rightV <- evalExpr rightExpr
    rightV `onSuccessEval` ((helper operator.typ) &> return)
  where
    helper Bang            v = succeed $ BooleanV $ not $ asBool v
    helper Minus (NumberV d) = succeed $ NumberV $ -d
    helper _               _ = fail $ EvalError OperandMustBeNumber operator

evalWhile :: Expr -> Statement -> Evaluating
evalWhile predExpr body =
  do
    predV <- evalExpr predExpr
    predV `onSuccessEval` (
      \predicate ->
        do
          if asBool predicate then do
            resultV <- evalStatement body
            resultV `onSuccessEval` (const $ evalWhile predExpr body)
          else
            win Nada
      )

evalDeclaration :: Variable -> Expr -> Evaluating
evalDeclaration (Variable varName _) expr =
  do
    valueV <- evalExpr expr
    valueV `onSuccessEval` (
        \value -> do
          modify $ declareVar varName value
          nothing
      )

evalPrint :: Expr -> Evaluating
evalPrint expr =
  do
    valueV <- evalExpr expr
    valueV `onSuccessEval` (
      \value -> do
        let text = showText $ if value == Nada then NilV else value
        void $ runEffect $ Print text
        nothing
      )

runTopLevelStatements :: [Statement] -> Evaluating
runTopLevelStatements statements =
  do
    mapM_ hoistIfFn statements
    runStatements statements
  where
    hoistIfFn (FunctionStatement fn) = modify $ declareVar fn.fnDecl.varName Nada
    hoistIfFn                      _ = return ()

runStatements :: [Statement] -> Evaluating
runStatements = foldM helper $ succeed Nada
  where
    helper acc s = acc `onSuccessEval` (const $ evalStatement s)

lookupVar :: Variable -> Evaluating
lookupVar (Variable varName vnToken) =
  do
    valM <- gets $ getVar varName
    maybe (lose $ EvalError (UnknownVariable varName) vnToken) win valM

setVariable :: Variable -> Value -> Evaluating
setVariable var value =
  do
    varV <- lookupVar var
    varV `onSuccessEval` (
      const $ do
        setVar var.varName value
        win value
      )

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

onSuccessEval :: Result CF.ControlFlow -> (Value -> Evaluating) -> Evaluating
onSuccessEval vali f = vali `failOrM` runIfNormal
  where
    runIfNormal (CF.Normal v) = f v
    runIfNormal             x = return $ Success x

onSuccessEval2 :: Result (CF.ControlFlow, CF.ControlFlow) -> ((Value, Value) -> Evaluating) -> Evaluating
onSuccessEval2 vali f = vali `failOrM` runIfNormal
  where
    runIfNormal ((CF.Normal v), (CF.Normal w)) = f (v, w)
    runIfNormal ((CF.Normal _),             x) = return $ Success x
    runIfNormal (            x,             _) = return $ Success x

onSuccessEval2Seq :: Result (CF.ControlFlow, [CF.ControlFlow]) -> ((Value, [Value]) -> Evaluating) -> Evaluating
onSuccessEval2Seq vali f = vali `failOrM` runIfNormal
  where
    runIfNormal ((CF.Normal v), ws) = either (Success &> return) ((v, ) &> f) $ purify [] ws
    runIfNormal (            x,  _) = return $ Success x

    purify acc                [] = Right $ List.reverse acc
    purify acc ((CF.Normal v):t) = purify (v:acc) t
    purify   _ (            h:_) = Left h

asObject :: Value -> EvalError -> Result V.Object
asObject (ObjectV obj) _ = Success obj
asObject             _ e = Failure $ NE.singleton e

nothing :: Evaluating
nothing = win Nada

fail :: EvalError -> Result a
fail err = Failure $ NE.singleton err

succeed :: Value -> Result CF.ControlFlow
succeed = CF.Normal &> Success

win :: Value -> Evaluating
win = succeed &> return

lose :: EvalError -> Prog a
lose = fail &> return
