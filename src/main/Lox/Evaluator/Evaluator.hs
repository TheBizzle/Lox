module Lox.Evaluator.Evaluator(eval) where

import Control.Monad.State(get, modify)

import Lox.Parser.Program(
    Expr(Assign, Binary, Call, Get, Grouping, LiteralExpr, Logical, Set, Super, This, Unary, Variable),
    Literal(BooleanLit, DoubleLit, NilLit, StringLit),
    Program(statements),
    Statement(Block, DeclareVar, ExpressionStatement, IfElse, PrintStatement, WhileStatement)
  )

import Lox.Scanner.Token(
    TokenPlus(token, TokenPlus),
    Token(And, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star)
  )

import Lox.Evaluator.Internal.Effect(Effect(Print))
import Lox.Evaluator.Internal.Type(typecheck)
import Lox.Evaluator.Internal.Value(Value(BooleanV, NumberV, StringV, NilV))

import Lox.Evaluator.Internal.EvalError(
    EvalError(NotImplemented, TypeError, UnknownVariable)
  )

import Lox.Evaluator.Internal.World(
    declareVar, defineFunction, getVar, popScope, pushScope, runEffect, setVar, World
  )

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE

import qualified Lox.Evaluator.Internal.ControlFlow as CF


type Result  t  = Validation (NonEmpty EvalError) t
type Worldly t  = World (Result t)
type Evaluated  = Worldly Value
type Evaluating = Worldly CF.ControlFlow

eval :: Program -> Evaluated
eval = statements &> runStatements &> evaluated

evaluated :: Evaluating -> Evaluated
evaluated = (<&> (validation Failure helper))
  where
    helper (CF.Exception    ex) = Failure $ NE.singleton ex
    helper (CF.Normal    value) = Success value

evalStatement :: Statement -> Evaluating
evalStatement (Block               statements          ) = evalBlock statements
evalStatement (DeclareVar          name       _    expr) = evalDeclaration name expr
evalStatement (ExpressionStatement                 expr) = evalExpr expr
evalStatement (IfElse              ant        con  alt ) = evalIfElse ant con alt
evalStatement (PrintStatement      _               expr) = evalPrint expr
evalStatement (WhileStatement      pred       stmt     ) = evalWhile pred stmt

evalExpr :: Expr -> Evaluating
evalExpr (Assign      name token value)        = evalAssign name token value
evalExpr (Binary      left operator right)     = evalBinary left operator right
evalExpr (Call        callee paren arguments)  = unimplemented paren
evalExpr (Get         object name token)       = unimplemented token
evalExpr (Grouping    expression)              = evalExpr expression
evalExpr (LiteralExpr literal _)               = win $ evalLiteral literal
evalExpr (Logical     left operator right)     = evalLogical left operator right
evalExpr (Set         object name token value) = unimplemented token
evalExpr (Super       keyword method)          = unimplemented keyword
evalExpr (This        keyword)                 = unimplemented keyword
evalExpr (Unary       operator right)          = evalUnary operator right
evalExpr (Variable    name token)              = lookupVar name token

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
    helper (NumberV l) Slash        (NumberV r) = num  l (/#) r
    helper (NumberV l) Star         (NumberV r) = num  l (* ) r
    helper l           _            r           = typeError operator [l, r]

    bool = succ BooleanV
    num  = succ NumberV
    str  = succ StringV

    succ consV l op r = Success $ CF.Normal $ consV $ l `op` r

evalBlock :: [Statement] -> Evaluating
evalBlock statements =
  do
    modify pushScope
    result <- runStatements statements
    modify popScope
    return result

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

evalUnary :: TokenPlus -> Expr -> Evaluating
evalUnary operator rightExpr =
  do
    rightV <- evalExpr rightExpr
    rightV `onSuccessEval` ((helper operator.token) &> return)
  where
    helper Bang  v           = succeed $ BooleanV $ not $ asBool v
    helper Minus (NumberV d) = succeed $ NumberV $ -d
    helper _     v           = typeError operator [v]

evalWhile :: Expr -> Statement -> Evaluating
evalWhile predExpr body =
  do
    predV <- evalExpr predExpr
    predV `onSuccessEval` (
      \predicate ->
        do
          when (asBool predicate) $ do
            resultV <- evalStatement body
            void $ resultV `onSuccessEval` (const $ evalWhile predExpr body)
          nothing
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
        void $ runEffect $ Print $ showText value
        nothing
      )

runStatements :: [Statement] -> Evaluating
runStatements = foldM helper $ succeed NilV
  where
    helper acc s = acc `onSuccessEval` (const $ evalStatement s)

lookupVar :: Text -> TokenPlus -> Evaluating
lookupVar varName vnTP =
  do
    w <- get
    let valM = getVar varName w
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

typeError :: TokenPlus -> [Value] -> Result a
typeError tp args = Failure $ NE.singleton $ TypeError tp $ catMaybes $ typecheck tp.token args

asBool :: Value -> Bool
asBool NilV             = False
asBool (BooleanV False) = False
asBool _                = True

onSuccessEval :: Result CF.ControlFlow -> (Value -> Evaluating) -> Evaluating
onSuccessEval vali f = validation (Failure &> return) runIfNormal vali
  where
    runIfNormal (CF.Normal v) = f v
    runIfNormal             x = return $ Success x

onSuccessEval2 :: Result (CF.ControlFlow, CF.ControlFlow) -> ((Value, Value) -> Evaluating) -> Evaluating
onSuccessEval2 vali f = validation (Failure &> return) runIfNormal vali
  where
    runIfNormal ((CF.Normal v), (CF.Normal w)) = f (v, w)
    runIfNormal ((CF.Normal _),             x) = return $ Success x
    runIfNormal (            x,             _) = return $ Success x

onSuccessEval2Seq :: Result (CF.ControlFlow, [CF.ControlFlow]) -> ((Value, [Value]) -> Evaluating) -> Evaluating
onSuccessEval2Seq vali f = validation (Failure &> return) runIfNormal vali
  where
    runIfNormal ((CF.Normal v), ws) = either (Success &> return) ((v, ) &> f) $ purify [] ws
    runIfNormal (            x,  _) = return $ Success x

    purify acc                [] = Right $ List.reverse acc
    purify acc ((CF.Normal v):t) = purify (v:acc) t
    purify   _ (            h:_) = Left h

nothing :: Evaluating
nothing = win NilV

fail :: EvalError -> Result CF.ControlFlow
fail err = Failure $ NE.singleton err

succeed :: Value -> Result CF.ControlFlow
succeed = CF.Normal &> Success

win :: Value -> Evaluating
win = succeed &> return

lose :: EvalError -> Evaluating
lose = fail &> return

unimplemented :: TokenPlus -> Evaluating
unimplemented = NotImplemented &> lose
