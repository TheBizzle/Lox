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


type Evaluated = World (Validation (NonEmpty EvalError) Value)

eval :: Program -> Evaluated
eval = statements &> runStatements

evalStatement :: Statement -> Evaluated
evalStatement (         DeclareVar name vnTP expr) = evalDeclaration name vnTP expr
evalStatement (ExpressionStatement           expr) = evalExpr expr
evalStatement (     PrintStatement         _ expr) = evalPrint expr

evalExpr :: Expr -> Evaluated
evalExpr (Assign      name token value)        = evalAssign name token value
evalExpr (Binary      left operator right)     = evalBinary left operator right
evalExpr (Call        callee paren arguments)  = unimplemented paren
evalExpr (Get         object name token)       = unimplemented token
evalExpr (Grouping    expression)              = evalExpr expression
evalExpr (LiteralExpr literal _)               = return $ Success $ evalLiteral literal
evalExpr (Logical     left operator right)     = unimplemented operator
evalExpr (Set         object name token value) = unimplemented token
evalExpr (Super       keyword method)          = unimplemented keyword
evalExpr (This        keyword)                 = unimplemented keyword
evalExpr (Unary       operator right)          = (evalExpr right) <&> (flip bindValidation $ evalUnary operator)
evalExpr (Variable    name token)              = lookupVar name token

evalAssign :: Text -> TokenPlus -> Expr -> Evaluated
evalAssign name token value = (evalExpr value) >>= (flip onSuccessEval $ \v -> setVariable name v token)

evalBinary :: Expr -> TokenPlus -> Expr -> Evaluated
evalBinary left operator right = helper <$> (evalExpr left) <*> (evalExpr right)
  where
    helper lv rv = lv `bindValidation` (\l -> rv `bindValidation` (helper2 l operator))

    helper2 l tp r = helper3 l tp.token r
      where
        helper3 l           BangEqual    r           = bool l (/=) r
        helper3 l           EqualEqual   r           = bool l (==) r
        helper3 (NumberV l) Greater      (NumberV r) = bool l (> ) r
        helper3 (NumberV l) GreaterEqual (NumberV r) = bool l (>=) r
        helper3 (NumberV l) Less         (NumberV r) = bool l (< ) r
        helper3 (NumberV l) LessEqual    (NumberV r) = bool l (<=) r
        helper3 (NumberV l) Minus        (NumberV r) = num  l (- ) r
        helper3 (NumberV l) Plus         (NumberV r) = num  l (+ ) r
        helper3 (StringV l) Plus         (StringV r) = str  l (<>) r
        helper3 (NumberV l) Slash        (NumberV r) = num  l (/#) r
        helper3 (NumberV l) Star         (NumberV r) = num  l (* ) r
        helper3 l           t            r           = typeError tp [l, r]

    bool = succeed BooleanV
    num  = succeed NumberV
    str  = succeed StringV

    succeed consV l op r = Success $ consV $ l `op` r

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

evalDeclaration :: Text -> TokenPlus -> Expr -> Evaluated
evalDeclaration varName vnTP expr =
  do
    valueV <- evalExpr expr
    valueV `onSuccessEval` (
        \value -> do
          modify $ setVar varName value
          return $ Success NilV
      )

evalPrint :: Expr -> Evaluated
evalPrint expr =
  do
    valueV <- evalExpr expr
    sequence $ flip second valueV $ \value -> do
      modify $ \s -> s { effects = (Print $ showText value) : s.effects }
      return NilV

runStatements :: [Statement] -> Evaluated
runStatements = foldM helper $ Success NilV
  where
    helper acc s = acc `onSuccessEval` (const $ evalStatement s)

lookupVar :: Text -> TokenPlus -> Evaluated
lookupVar varName vnTP =
  do
    w <- get
    let valM = getVar varName w
    return $ maybe (fail $ UnknownVariable vnTP varName) Success valM

setVariable :: Text -> Value -> TokenPlus -> Evaluated
setVariable varName value vnTP =
  do
    varV <- lookupVar varName vnTP
    sequence $ flip second varV $ const $
      do
        modify $ setVar varName value
        return value

typeError :: TokenPlus -> [Value] -> Validation (NonEmpty EvalError) a
typeError tp args = Failure $ NE.singleton $ TypeError tp $ catMaybes $ typecheck tp.token args

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

onSuccessEval :: (Validation (NonEmpty EvalError) a) -> (a -> Evaluated) -> Evaluated
onSuccessEval vali f = validation (Failure &> return) f vali

fail :: EvalError -> Validation (NonEmpty EvalError) Value
fail err = Failure $ NE.singleton err

unimplemented :: TokenPlus -> Evaluated
unimplemented = NotImplemented &> fail &> return
