module Lox.Evaluator.Evaluator(eval) where

import Control.Monad.State(get, gets, modify)

import Lox.Parser.AST(
    AST(statements),
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, name, Set, Super, This, Unary, Variable),
    exprToToken,
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Statement(Block, Class, DeclareVar, ExpressionStatement, Function, IfElse, PrintStatement, ReturnStatement, WhileStatement)
  )

import Lox.Scanner.Token(
    TokenPlus(token, TokenPlus),
    Token(And, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.Value(Value(BooleanV, ClassV, FunctionV, Nada, NilV, NumberV, ObjectV, StringV))

import Lox.Evaluator.Internal.EvalError(
    EvalError(ArityMismatch, CanOnlyGetObj, CanOnlyRefThisInsideClass, CanOnlySetObj, ClassesCanOnlyContainFns, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, NotCallable, SuperCannotBeSelf, SuperMustBeAClass, TopLevelReturn, UnknownVariable)
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
eval = statements &> runStatements &> evaluated

evaluated :: Evaluating -> Evaluated
evaluated = (<&> (validation Failure helper))
  where
    helper (CF.Exception    ex) = Failure $ NE.singleton ex
    helper (CF.Normal    value) = Success value
    helper (CF.Return        _) = Failure $ NE.singleton TopLevelReturn

evalStatement :: Statement -> Evaluating
evalStatement (Block               statements             ) = evalBlock statements
evalStatement (Class               name       tp   sntm ms) = evalClass name tp sntm ms
evalStatement (DeclareVar          name       _    expr   ) = evalDeclaration name expr
evalStatement (ExpressionStatement                 expr   ) = evalExpr expr
evalStatement (IfElse              ant        con  alt    ) = evalIfElse ant con alt
evalStatement (PrintStatement      _               expr   ) = evalPrint expr
evalStatement (ReturnStatement     _          expM        ) = evalReturn expM
evalStatement (WhileStatement      pred       stmt        ) = evalWhile pred stmt
evalStatement (Function            tp         ps   body   ) = evalFunction tp ps body

evalExpr :: Expr -> Evaluating
evalExpr (Assign      name token value)    = evalAssign name token value
evalExpr (Binary      left operator right) = evalBinary left operator right
evalExpr (Call        callee _ arguments)  = evalCall callee arguments
evalExpr (Get         object name tp)      = evalGet object name tp
evalExpr (Grouping    expression)          = evalExpr expression
evalExpr (LiteralExpr literal _)           = win $ evalLiteral literal
evalExpr (Logical     left operator right) = evalLogical left operator right
evalExpr (Set         object name t value) = evalSet object name value t
evalExpr (Super       keyword method _)    = indexSuper keyword method
evalExpr (This        keyword)             = evalThis keyword
evalExpr (Unary       operator right)      = evalUnary operator right
evalExpr (Variable    name token)          = lookupVar name token

evalAssign :: Text -> TokenPlus -> Expr -> Evaluating
evalAssign name token value = (evalExpr value) >>= (flip onSuccessEval $ \v -> setVariable name v token)

evalBinary :: Expr -> TokenPlus -> Expr -> Evaluating
evalBinary left operator right =
    do
      lv <- evalExpr left
      rv <- evalExpr right
      ((,) <$> lv <*> rv) `onSuccessEval2` (
        \(l, r) -> return $ helper l operator.token r
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
    helper _           Plus         _           = fail $ OperandsMustBeNumsOrStrs operator
    helper (NumberV l) Slash        (NumberV r) = num  l (/#) r
    helper (NumberV l) Star         (NumberV r) = num  l (* ) r
    helper _           _            _           = fail $ OperandsMustBeNumbers operator

    bool = succ BooleanV
    num  = succ NumberV
    str  = succ StringV

    succ consV l op r = Success $ CF.Normal $ consV $ l `op` r

evalBlock :: [Statement] -> Evaluating
evalBlock statements =
  do
    env    <- currentEnvironment
    modify $ pushScope env
    result <- runStatements statements
    modify popScope
    return result

evalCall :: Expr -> [Expr] -> Evaluating
evalCall callableExpr argExprs =
  do
    callableV <- evalExpr callableExpr
    argVs     <- forM argExprs evalExpr
    ((,) <$> callableV <*> (sequenceA argVs)) `onSuccessEval2Seq` helper
  where
    helper (callableV, args) =
      case (arity callableV, callableV) of
        (Nothing  ,               _)                         -> lose $ NotCallable $ exprToToken callableExpr
        (Just arty,               _) | doesntMatch arty args -> lose $ ArityMismatch (exprToToken callableExpr) arty $ numGotten args
        (        _, FunctionV  _ fn)                         -> runFunction evalStatement fn.idNum    args
        (        _, ClassV    clazz)                         -> initObject  evalStatement clazz.cName args
        x                                                    -> error $ "This isn't the callable we're looking for: " <> (showText x)

    doesntMatch arity args = arity /= (numGotten args)

    numGotten args = args |> length &> fromIntegral

evalClass :: Text -> TokenPlus -> Maybe (Text, TokenPlus) -> [Statement] -> Evaluating
evalClass className classTP superNameTokenM methods =
  do
    superClassMV      <- processSuperPair superNameTokenM
    tripleVs          <- forM methods asTriple
    let triplesSuperV  = (,) <$> (sequenceA tripleVs) <*> superClassMV
    triplesSuperV `failOrM` defClass
  where
    asTriple (Function tp ps body) = return $ Success (tp.name, map name ps, body)
    asTriple _                     = lose $ ClassesCanOnlyContainFns classTP

    processSuperPair Nothing                               = return $ Success Nothing
    processSuperPair (Just (name, tp)) | name == className = lose $ SuperCannotBeSelf tp name
    processSuperPair (Just (name, tp))                     =
      do
        valM <- gets $ getVar name
        maybe (lose $ UnknownVariable tp name) processSuperValue valM
      where
        processSuperValue (ClassV clazz) = return $ Success $ Just clazz
        processSuperValue              _ = lose $ SuperMustBeAClass tp name

    defClass (triples, superClassM) =
      do
        void $ defineClass className superClassM triples
        nothing

evalFunction :: Expr -> [Expr] -> [Statement] -> Evaluating
evalFunction nameExpr paramExprs body =
  do
    void $ defineFunction fnName params body
    nothing
  where
    fnName = nameExpr.name
    params = map name paramExprs

evalGet :: Expr -> Text -> TokenPlus -> Evaluating
evalGet objectExpr propName tp =
  do
    valueV <- evalExpr objectExpr
    valueV `onSuccessEval` (
        \v -> (asObject v $ CanOnlyGetObj tp) `failOrM` (\x -> indexObject tp x propName)
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

evalLogical :: Expr -> TokenPlus -> Expr -> Evaluating
evalLogical left operator right =
  do
    lv <- evalExpr left
    lv `onSuccessEval` (
      \l -> case operator of
              (TokenPlus And _) -> if (not . asBool $ l) then return (succeed l) else evalExpr right
              (TokenPlus  Or _) -> if (      asBool   l) then return (succeed l) else evalExpr right
              tp                -> error $ "Impossible logical operator: " <> (showText tp)
      )

evalReturn :: Maybe Expr -> Evaluating
evalReturn exprM = maybe (return $ Success $ CF.Return $ Nada) (evalExpr >=> helper) exprM
  where
    helper = flip failOrM $ \case
      ex@(CF.Exception _) -> return $ Success ex
      (   CF.Normal    v) -> (transferOwnership v) $> (Success $ CF.Return v)
      (   CF.Return    _) -> error "`return return x;` should not be possible!"

evalSet :: Expr -> Text -> Expr -> TokenPlus -> Evaluating
evalSet objectExpr propName valueExpr tp =
  do
    objValV <- evalExpr objectExpr
    anyValV <- evalExpr valueExpr
    ((,) <$> objValV <*> anyValV) `onSuccessEval2` (
        \(objVal, anyVal) -> (asObject objVal $ CanOnlySetObj tp) `failOrM` (\obj -> assignIntoObject obj propName anyVal)
      )

evalThis :: TokenPlus -> Evaluating
evalThis keyword = get >>= ((getVar "this") &> maybe (lose $ CanOnlyRefThisInsideClass keyword) win)

evalUnary :: TokenPlus -> Expr -> Evaluating
evalUnary operator rightExpr =
  do
    rightV <- evalExpr rightExpr
    rightV `onSuccessEval` ((helper operator.token) &> return)
  where
    helper Bang            v = succeed $ BooleanV $ not $ asBool v
    helper Minus (NumberV d) = succeed $ NumberV $ -d
    helper _               _ = fail $ OperandMustBeNumber operator

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

evalDeclaration :: Text -> Expr -> Evaluating
evalDeclaration varName expr =
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

runStatements :: [Statement] -> Evaluating
runStatements = foldM helper $ succeed Nada
  where
    helper acc s = acc `onSuccessEval` (const $ evalStatement s)

lookupVar :: Text -> TokenPlus -> Evaluating
lookupVar varName vnTP =
  do
    valM <- gets $ getVar varName
    maybe (lose $ UnknownVariable vnTP varName) win valM

setVariable :: Text -> Value -> TokenPlus -> Evaluating
setVariable varName value vnTP =
  do
    varV <- lookupVar varName vnTP
    varV `onSuccessEval` (
      const $ do
        modify $ setVar varName value
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
