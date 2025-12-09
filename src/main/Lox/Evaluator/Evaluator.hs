{-# LANGUAGE OverloadedRecordDot #-}

module Lox.Evaluator.Evaluator(eval) where

import Control.Lens((#))
import Control.Monad.State(modify)

import Data.List.NonEmpty(NonEmpty)
import Data.Validation(bindValidation, _Failure, _Success, Validation)

import Lox.Parser.Program(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, Variable),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(statements),
    Statement(ExpressionStatement, PrintStatement)
  )

import Lox.Scanner.Token(
    TokenPlus(token),
    Token(Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Plus, Slash, Star)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.EvalError(EvalError(NotImplemented, TypeError))
import Lox.Evaluator.Internal.Type(typecheck)
import Lox.Evaluator.Internal.Value(Value(BooleanV, NumberV, StringV, NilV))
import Lox.Evaluator.Internal.World(World, WorldState(effects))

import qualified Data.List.NonEmpty as NE


eval :: Program -> World (Validation (NonEmpty EvalError) Value)
eval = statements &> (flip foldM (_Success # NilV) $ \_ s -> evalStatement s)

evalStatement :: Statement -> World (Validation (NonEmpty EvalError) Value)
evalStatement (ExpressionStatement   expr) = evalExpr expr
evalStatement (     PrintStatement _ expr) =
  do
    valueV <- evalExpr expr
    sequence $ flip second valueV $ \value -> do
      modify $ \s -> s { effects = (Print $ showText value) : s.effects }
      return NilV

evalExpr :: Expr -> World (Validation (NonEmpty EvalError) Value)
evalExpr (Assign      name value)             = fail $ NotImplemented name
evalExpr (Binary      left operator right)    = handleBinary <$> (evalExpr left) <*> (evalExpr right)
  where
    handleBinary lv rv = lv `bindValidation` (\l -> rv `bindValidation` (evalBinary l operator))
evalExpr (Call        callee paren arguments) = fail $ NotImplemented paren
evalExpr (Get         object name)            = fail $ NotImplemented name
evalExpr (Grouping    expression)             = evalExpr expression
evalExpr (LiteralExpr literal)                = return $ _Success # (evalLiteral literal)
evalExpr (Logical     left operator right)    = fail $ NotImplemented operator
evalExpr (Set         object name value)      = fail $ NotImplemented name
evalExpr (Super       keyword method)         = fail $ NotImplemented keyword
evalExpr (This        keyword)                = fail $ NotImplemented keyword
evalExpr (Unary       operator right)         = (evalExpr right) <&> (flip bindValidation $ evalUnary operator)
evalExpr (Variable    name)                   = fail $ NotImplemented name

evalLiteral :: Literal -> Value
evalLiteral (BooleanLit bool)   = BooleanV bool
evalLiteral (DoubleLit  double) =  NumberV double
evalLiteral (StringLit  str)    =  StringV str
evalLiteral  NilLit             =     NilV

evalUnary :: TokenPlus -> Value -> Validation (NonEmpty EvalError) Value
evalUnary tp v = helper tp.token v
  where
    helper Bang  v           = _Success # (BooleanV $ not $ asBool v)
    helper Minus (NumberV d) = _Success # (NumberV $ -d)
    helper t     v           = typeError tp [v]

evalBinary :: Value -> TokenPlus -> Value -> Validation (NonEmpty EvalError) Value
evalBinary l tp r = helper l tp.token r
  where
    helper l           BangEqual    r           = _Success # (BooleanV $ l  /= r)
    helper l           EqualEqual   r           = _Success # (BooleanV $ l  == r)
    helper (NumberV l) Greater      (NumberV r) = _Success # (BooleanV $ l  >  r)
    helper (NumberV l) GreaterEqual (NumberV r) = _Success # (BooleanV $ l  >= r)
    helper (NumberV l) Less         (NumberV r) = _Success # (BooleanV $ l  <  r)
    helper (NumberV l) LessEqual    (NumberV r) = _Success # (BooleanV $ l  <= r)
    helper (NumberV l) Minus        (NumberV r) = _Success # ( NumberV $ l  -  r)
    helper (NumberV l) Plus         (NumberV r) = _Success # ( NumberV $ l  +  r)
    helper (StringV l) Plus         (StringV r) = _Success # ( StringV $ l  <> r)
    helper (NumberV l) Slash        (NumberV r) = _Success # ( NumberV $ l  /  r)
    helper (NumberV l) Star         (NumberV r) = _Success # ( NumberV $ l  *  r)
    helper l           t            r           = typeError tp [l, r]

typeError :: TokenPlus -> [Value] -> Validation (NonEmpty EvalError) a
typeError tp args = _Failure # (NE.singleton $ TypeError tp $ catMaybes $ typecheck tp.token args)

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

fail :: EvalError -> World (Validation (NonEmpty EvalError) Value)
fail err = return $ _Failure # (NE.singleton err)
