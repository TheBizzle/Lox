module Lox.Evaluator.Evaluator(eval) where

import Control.Monad.State(get, modify)

import Lox.Parser.Program(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, Variable),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(statements),
    Statement(DeclareVar, ExpressionStatement, PrintStatement)
  )

import Lox.Scanner.Token(
    TokenPlus(token, TokenPlus),
    Token(Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Identifier, Less, LessEqual, Minus, Plus, Slash, Star)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.EvalError(EvalError(NotImplemented, TypeError, UnknownVariable))
import Lox.Evaluator.Internal.Type(typecheck)
import Lox.Evaluator.Internal.Value(Value(BooleanV, NumberV, StringV, NilV))
import Lox.Evaluator.Internal.World(getVar, setVar, World, WorldState(effects))

import qualified Data.List.NonEmpty as NE


eval :: Program -> World (Validation (NonEmpty EvalError) Value)
eval = statements &> (flip foldM (Success NilV) $ \_ s -> evalStatement s)

evalStatement :: Statement -> World (Validation (NonEmpty EvalError) Value)
evalStatement (DeclareVar       vnTP expr) = evalDeclaration vnTP expr
evalStatement (ExpressionStatement   expr) = evalExpr expr
evalStatement (     PrintStatement _ expr) = evalPrint expr

evalExpr :: Expr -> World (Validation (NonEmpty EvalError) Value)
evalExpr (Assign      name value)             = fail_ $ NotImplemented name
evalExpr (Binary      left operator right)    = handleBinary <$> (evalExpr left) <*> (evalExpr right)
  where
    handleBinary lv rv = lv `bindValidation` (\l -> rv `bindValidation` (evalBinary l operator))
evalExpr (Call        callee paren arguments) = fail_ $ NotImplemented paren
evalExpr (Get         object name)            = fail_ $ NotImplemented name
evalExpr (Grouping    expression)             = evalExpr expression
evalExpr (LiteralExpr literal _)              = return $ Success $ evalLiteral literal
evalExpr (Logical     left operator right)    = fail_ $ NotImplemented operator
evalExpr (Set         object name value)      = fail_ $ NotImplemented name
evalExpr (Super       keyword method)         = fail_ $ NotImplemented keyword
evalExpr (This        keyword)                = fail_ $ NotImplemented keyword
evalExpr (Unary       operator right)         = (evalExpr right) <&> (flip bindValidation $ evalUnary operator)
evalExpr (Variable    name)                   = lookupVar name

evalLiteral :: Literal -> Value
evalLiteral (BooleanLit bool)   = BooleanV bool
evalLiteral (DoubleLit  double) =  NumberV double
evalLiteral (StringLit  str)    =  StringV str
evalLiteral  NilLit             =     NilV

evalUnary :: TokenPlus -> Value -> Validation (NonEmpty EvalError) Value
evalUnary tp v = helper tp.token v
  where
    helper Bang  v           = Success $ BooleanV $ not $ asBool v
    helper Minus (NumberV d) = Success $ NumberV $ -d
    helper t     v           = typeError tp [v]

evalBinary :: Value -> TokenPlus -> Value -> Validation (NonEmpty EvalError) Value
evalBinary l tp r = helper l tp.token r
  where
    helper l           BangEqual    r           = Success $ BooleanV $ l  /= r
    helper l           EqualEqual   r           = Success $ BooleanV $ l  == r
    helper (NumberV l) Greater      (NumberV r) = Success $ BooleanV $ l  >  r
    helper (NumberV l) GreaterEqual (NumberV r) = Success $ BooleanV $ l  >= r
    helper (NumberV l) Less         (NumberV r) = Success $ BooleanV $ l  <  r
    helper (NumberV l) LessEqual    (NumberV r) = Success $ BooleanV $ l  <= r
    helper (NumberV l) Minus        (NumberV r) = Success $  NumberV $ l  -  r
    helper (NumberV l) Plus         (NumberV r) = Success $  NumberV $ l  +  r
    helper (StringV l) Plus         (StringV r) = Success $  StringV $ l  <> r
    helper (NumberV l) Slash        (NumberV r) = Success $  NumberV $ l  /# r
    helper (NumberV l) Star         (NumberV r) = Success $  NumberV $ l  *  r
    helper l           t            r           = typeError tp [l, r]

evalDeclaration :: TokenPlus -> Expr -> World (Validation (NonEmpty EvalError) Value)
evalDeclaration vnTP expr =
  do
    valueV <- evalExpr expr
    let varName = extractVarName vnTP
    sequence $ flip second valueV $ \value -> do
      modify $ setVar varName value
      return NilV

evalPrint :: Expr -> World (Validation (NonEmpty EvalError) Value)
evalPrint expr =
  do
    valueV <- evalExpr expr
    sequence $ flip second valueV $ \value -> do
      modify $ \s -> s { effects = (Print $ showText value) : s.effects }
      return NilV

lookupVar :: TokenPlus -> World (Validation (NonEmpty EvalError) Value)
lookupVar vnTP =
  do
    w <- get
    let varName = extractVarName vnTP
    let valM    = getVar varName w
    return $ maybe (fail $ UnknownVariable vnTP varName) Success valM

extractVarName :: TokenPlus -> Text
extractVarName (TokenPlus (Identifier vn) _) = vn
extractVarName                             _ = error "Impossible condition: Varname TP is not an identifier"

typeError :: TokenPlus -> [Value] -> Validation (NonEmpty EvalError) a
typeError tp args = Failure $ NE.singleton $ TypeError tp $ catMaybes $ typecheck tp.token args

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

fail_ :: EvalError -> World (Validation (NonEmpty EvalError) Value)
fail_ = fail &> return

fail :: EvalError -> Validation (NonEmpty EvalError) Value
fail err = Failure $ NE.singleton err
