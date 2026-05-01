module Lox.Evaluator.Internal.Eval.Expr(evalExpr) where

import Control.Monad.State(get, gets)

import Lox.Scanner.Token(
    Token(Token, typ)
  , TokenType(And, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star)
  )

import Lox.Parser.AST(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, VarRef)
  , exprToToken
  , Literal(BooleanLit, DoubleLit, NilLit, StringLit)
  , Statement
  , Variable(Variable, varName)
  )

import Lox.Evaluator.Internal.Function(runFunction)
import Lox.Evaluator.Internal.OOP(assignIntoObject, classArity, indexObject, indexSuper, initObject)
import Lox.Evaluator.Internal.Program(Evaluating)

import Lox.Evaluator.Internal.Value(
    argNames
  , Class(cName)
  , Function(idNum)
  , Value(BooleanV, ClassV, FunctionV, NilV, NumberV, StringV)
  )

import Lox.Evaluator.Internal.Variable(getVar, setVar)

import Lox.Evaluator.Internal.EvalError(
    EvalError(EvalError)
  , EvalErrorType(ArityMismatch, CanOnlyGetObj, CanOnlySetObj, OperandMustBeNumber, OperandsMustBeNumbers, OperandsMustBeNumsOrStrs, NotCallable, UnknownVariable)
  )

import Lox.Evaluator.Internal.Eval.Common(
    asBool, asObject, fail, lose, onSuccessEval, onSuccessEval2, onSuccessEval2Seq, succeed, win
  )

import qualified Lox.Evaluator.Internal.ControlFlow as CF


type EvalStmt = Statement -> Evaluating

evalExpr :: EvalStmt -> Expr -> Evaluating
evalExpr evalStmt (Assign      var value)              = evalAssign evalStmt var value
evalExpr evalStmt (Binary      left operator right)    = evalBinary evalStmt left operator right
evalExpr evalStmt (Call        callee token arguments) = evalCall evalStmt callee token arguments
evalExpr evalStmt (Get         object var)             = evalGet evalStmt object var
evalExpr evalStmt (Grouping    expression)             = evalExpr evalStmt expression
evalExpr _        (LiteralExpr literal _)              = win $ evalLiteral literal
evalExpr evalStmt (Logical     left operator right)    = evalLogical evalStmt left operator right
evalExpr evalStmt (Set         object var value)       = evalSet evalStmt object var value
evalExpr _        (Super       keyword var)            = indexSuper keyword var
evalExpr _        (This        keyword)                = evalThis keyword
evalExpr evalStmt (Unary       operator right)         = evalUnary evalStmt operator right
evalExpr _        (VarRef      var)                    = lookupVar var

evalAssign :: EvalStmt -> Variable -> Expr -> Evaluating
evalAssign evalStmt var value = (evalExpr evalStmt value) >>= (flip onSuccessEval $ \v -> setVariable var v)

evalBinary :: EvalStmt -> Expr -> Token -> Expr -> Evaluating
evalBinary evalStmt left operator right =
    do
      lv <- evalExpr evalStmt left
      rv <- evalExpr evalStmt right
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

evalCall :: EvalStmt -> Expr -> Token -> [Expr] -> Evaluating
evalCall evalStmt callableExpr token argExprs =
  do
    callableV <- evalExpr evalStmt callableExpr
    argVs     <- forM argExprs $ evalExpr evalStmt
    ((,) <$> callableV <*> (sequenceA argVs)) `onSuccessEval2Seq` helper
  where
    helper (callableV, args) =
      case (arity callableV, callableV) of
        (Nothing  ,                 _)                         -> lose $ EvalError NotCallable $ exprToToken callableExpr
        (Just arty,                 _) | doesntMatch arty args -> lose $ EvalError (ArityMismatch arty $ numGotten args) $ exprToToken callableExpr
        (        _, FunctionV  _ fn _)                         -> runFunction       evalStmt fn.idNum    args
        (        _, ClassV      clazz)                         -> initObject  token evalStmt clazz.cName args
        x                                                      -> error $ "This isn't the callable we're looking for: " <> (showText x)

    doesntMatch arity args = arity /= (numGotten args)

    numGotten args = args |> length &> fromIntegral

evalGet :: EvalStmt -> Expr -> Variable -> Evaluating
evalGet evalStmt objectExpr (Variable propName token) =
  do
    valueV <- evalExpr evalStmt objectExpr
    valueV `onSuccessEval` (
        \v -> (asObject v $ EvalError CanOnlyGetObj token) `failOrM` (\x -> indexObject token x propName)
      )

evalLiteral :: Literal -> Value
evalLiteral (BooleanLit bool)   = BooleanV bool
evalLiteral (DoubleLit  double) =  NumberV double
evalLiteral (StringLit  str)    =  StringV str
evalLiteral  NilLit             =     NilV

evalLogical :: EvalStmt -> Expr -> Token -> Expr -> Evaluating
evalLogical evalStmt left operator right =
  do
    lv <- evalExpr evalStmt left
    lv `onSuccessEval` (
      \l -> case operator of
              (Token And _) -> if (not . asBool $ l) then return (succeed l) else evalExpr evalStmt right
              (Token  Or _) -> if (      asBool   l) then return (succeed l) else evalExpr evalStmt right
              token         -> error $ "Impossible logical operator: " <> (showText token)
      )

evalSet :: EvalStmt -> Expr -> Variable -> Expr -> Evaluating
evalSet evalStmt objectExpr (Variable propName token) valueExpr =
  do
    objValV <- evalExpr evalStmt objectExpr
    anyValV <- evalExpr evalStmt  valueExpr
    ((,) <$> objValV <*> anyValV) `onSuccessEval2` (
        \(objVal, anyVal) ->
          let obj    = (asObject objVal $ EvalError CanOnlySetObj token)
              dfault = (\obj -> assignIntoObject obj propName anyVal)
           in
              obj `failOrM` dfault
      )

evalThis :: Token -> Evaluating
evalThis _ = get >>= ((getVar "this") &> maybe (error msg) win)
  where
    msg =  "Can't reference `this` outside of class.  Verifier should have caught this!"

evalUnary :: EvalStmt -> Token -> Expr -> Evaluating
evalUnary evalStmt operator rightExpr =
  do
    rightV <- evalExpr evalStmt rightExpr
    rightV `onSuccessEval` ((helper operator.typ) &> return)
  where
    helper Bang            v = succeed $ BooleanV $ not $ asBool v
    helper Minus (NumberV d) = succeed $ NumberV $ -d
    helper _               _ = fail $ EvalError OperandMustBeNumber operator

arity :: Value -> Maybe Word
arity (FunctionV _ fn _) = fn |> argNames &> length &> fromIntegral &> Just
arity (ClassV         c) = c  |> classArity &> Just
arity _                  = Nothing

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
